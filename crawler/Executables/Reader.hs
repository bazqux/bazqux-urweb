-- пустой файл, нужен, т.к. Cabal требует main-is
-- main находится в Ur/Web
module Reader () where

import APIServer
-- важно импортировать хоть что-то из библиотеки bazqux, иначе при пересборке
-- библиотеки не будет пересобираться запускной файл
