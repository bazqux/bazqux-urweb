export function hashMapNew() { return { m : Object.create(null), size : 0 }; }
export function hashMapClear(m) { m.m = Object.create(null); m.size = 0; }
export function hashMapInsert(m) {
    return function (k) {
        return function (x) {
            return function () {
                if (hashMapMember(m,k)) {
                    m.m[k] = x;
                } else {
                    m.m[k] = x;
                    m.size++;
                }
            } } } }
export function hashMapDelete(m, k) {
    if (hashMapMember(m, k)) {
        delete m.m[k];
        m.size--;
    }
}
export function hashMapMember(m, k) {
    return Object.prototype.hasOwnProperty.call(m.m, k);
}
export function hashMapToList(m) {
    return function () {
        var r = null;
        Object.keys(m.m).forEach((key) => {
            r = { _1 : { _1 : key, _2 : m.m[key] }, _2 : r };
        })
        return r;
    } }
export function hashMapLookup(m) {
    return function (k) {
        return function () {
            return hashMapMember(m,k) ? m.m[k] : null;
        } } }
export function hashMapLookupDefault(m) {
    return function (d) {
        return function (k) {
            return function () {
                return hashMapMember(m,k) ? m.m[k] : d;
            } } } }
export function hashMapSize(m) { return m.size; }
