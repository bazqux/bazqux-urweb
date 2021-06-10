const path = require("path");
const webpack = require("webpack");
const autoprefixer = require('autoprefixer');
const zlib = require("zlib");

const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CompressionPlugin = require("compression-webpack-plugin");
const TerserPlugin = require("terser-webpack-plugin");
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin');
const ESLintPlugin = require('eslint-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin')
const exec = require('child_process').exec;

const eslintConfig = {
    "env": {
        "browser": true,
        "es6": true
    },
    "parser": "@babel/eslint-parser",
    "extends": "eslint:recommended",
    "parserOptions": {
        "requireConfigFile" : false,
    },
    "globals": {
        // после обновления надо чистить ./node_modules/.cache
        "require":false,
        "instgrm":false, "twttr":false, "opera":false,
        "fresh":false, "atr":false, "cat":false, "cs":false, "eh":false,
        "flattenLocal":false, "sc":false, "sv":false, "sr":false,
        "getXHR":false, "xhrFinished":false,
        "requestUri":false, "setInnerHTML":false,
        "makeSome":false, "populate":false,
        "execF":false, "uw_event":true, "uw_mouseEvent":false, "uw_debug":false,
        "uw_stopPropagation":false, "uw_preventDefault":false,
        "unloading":false, "strftime":false,
        "client_id":false, "client_pass":false, "sig":false,
        "whine":false, "inFlight":true, "cons":false,
        "servErr":false, "doExn":false, "er":false, "conn":false,
        "isLower":false, "isDigit":false,
        "maySuspend":false,
        "chrome":false, "id":false, "eq":false
    },
    "rules": {
        "no-empty": [
            "error",
            {
                "allowEmptyCatch": true
            }
        ]
    }
}

function plugins(argv) {
    var p = [
        new CompressionPlugin({
            test: /\.(js|css|map)$/,
            // gzip
        }),
//         new CompressionPlugin({
//             filename: "[path][base].br",
//             algorithm: "brotliCompress",
//             test: /\.(js|css|map)$/,
//             compressionOptions: {
//                 params: {
//                     [zlib.constants.BROTLI_PARAM_QUALITY]: 11,
//                 },
//             }
//         }),
        new MiniCssExtractPlugin({
            filename: "styles.[contenthash].css",
        }),
        new ESLintPlugin({
            cache: true,
            cacheLocation: path.join(__dirname, 'dist/.eslintcache'),
            baseConfig: eslintConfig
        }),
        new HtmlWebpackPlugin({
            filename: 'scripts.html',
            inject: false,
            scriptLoading: 'blocking',
            templateContent: ({htmlWebpackPlugin}) => `
            ${htmlWebpackPlugin.tags.bodyTags}
        `
        }),
        new HtmlWebpackPlugin({
            filename: 'styles.html',
            inject: false,
            templateContent: ({htmlWebpackPlugin}) => `
            ${htmlWebpackPlugin.tags.headTags.filter((tag) => tag.tagName === 'link')}
        `
        })
    ];

    if (argv.mode === 'development') {
        p.push({
            apply: (compiler) => {
                compiler.hooks.afterEmit.tap('AfterEmitPlugin', (compilation) => {
                    exec('killall Reader', (err, stdout, stderr) => {
                        if (stdout) process.stdout.write(stdout);
                        if (stderr) process.stderr.write(stderr);
                    });
                });
            }});
    }

    return p;
}

module.exports = (env, argv) => { return {
    entry: {
        "utils": "./js/main.js",
    },
    optimization: {
        minimizer: [
            new TerserPlugin(),
            new CssMinimizerPlugin()
        ],
        splitChunks: {
            cacheGroups: {
                defaultVendors: {
                    test: /node_modules\/(?!highlight\.js\/)|js\/(vendor|mq.genie|browser-info|country-code|hash-map|local-storage|css-parser)\.js/,
                    name: 'vendor',
                    chunks: 'all',
                },
                hljs: {
                    test: /node_modules\/highlight\.js|js\/hljs\.js/,
                    name: 'hljs',
                    chunks: 'all',
                }
            }
        }
    },
    performance : {
        hints : "warning",
        maxAssetSize: 1024*1024,
        maxEntrypointSize: 2048*1024
    },
    stats: {
        // optimizationBailout: true,
        modules: false,
        entrypoints: false,
        version: false,
    },
    watchOptions: {
        ignored: /node_modules/
    },
    devtool: "source-map",
    plugins: plugins(argv),
    output: {
        path: path.join(__dirname, 'dist/assets'),
        publicPath: '/assets/',
        filename: "[name].[contenthash].js",
        library: "bq",
        hashFunction: "sha3-512",
        hashDigestLength: 40,
    },
    resolve: {
        fallback: {
            "crypto": require.resolve('browserify-sign/algos'),
            //"crypto": require.resolve("crypto-browserify"),
            // ^ так тянет почти мегабайт кода, а нам нужен только MD5
            "buffer": require.resolve("buffer"),
            "stream": require.resolve("stream-browserify"),
        }
    },
    module: {
        rules: [{
            test: /\.js$/,
            exclude: /node_modules\/(?!highlight\.js\/)/,
            // HighlightJS содержит const, которые не работают в iOS 9
            use : {
                loader: "babel-loader",
                options: {
                    presets: [
                        ["@babel/preset-env",
                         {
                             useBuiltIns: "entry",
                             // ничего не делает с entry,
                             // требуется самому включать core-js
                             // что добавляет 150kb minified кода полифилов
                             // (а только core-js/es/object/entries 8kb)
                             // с usage добавляет 50kb minified кода
                             corejs: 3
                         }]
                    ]
                }
            }
        }, {
            test: /\.styl$/,
            use: [
                MiniCssExtractPlugin.loader,
                {
                    loader: 'css-loader',
                    options: {
                        url: false, // не копируем файлы через file-loader
                        import: false,
                    },
                },
                {
                    loader: 'postcss-loader',
                    options: {
                        postcssOptions: {
                            plugins: [
                                autoprefixer({ remove: false })
                            ]
                        },
                    }
                },
                {
                    loader: "stylus-loader",
                    options: {
                        stylusOptions: {
                            compress: false
                            // compress:true превращает
                            //   --my-var: 0px
                            // в
                            //   --my-var: 0,
                            // из-за чего она потом не работает в calc()
                        }
                    }
                }
            ]
//         }, {
//             test: /\.(woff2?|png)$/,
//             use: [
//                 {
//                     loader: 'file-loader',
//                     options: {
//                         name: '[name].[ext]',
//                     }
//                 }
//             ]
        }]
    }
}}
