import md5 from 'crypto-js/md5'
import { isMobile } from './browser-info'

const dummyLocalStorage = {};

function getLocalStorage() {
    return (typeof(Storage)!=="undefined" && window.localStorage != null) ? localStorage : dummyLocalStorage;
}

function storageKey(user, key)
{
    return "_" + md5(user) + (isMobile ? "_mobile_" : "_") + key;
}

function checkLocalStorageError(e) {
    if(e.name == "NS_ERROR_FILE_CORRUPTED") {
        alert("Sorry, it looks like your browser storage has been corrupted. Please clear your storage by going to Tools -> Clear Recent History -> Cookies and set time range to 'Everything'. This will remove the corrupted browser storage across all sites.");
    }
}

export function getFromLocalStorage(user, key, def) {
    try {
        const ls = getLocalStorage();
        key = storageKey(user,key);
        return ls[key] != null ? ls[key] : def;
    } catch (e) {
        checkLocalStorageError(e);
        return def;
    }
}

export function saveToLocalStorage(user, key, val)
{
    try {
        key = storageKey(user,key);
        getLocalStorage()[key] = val;
    } catch (e) {
        checkLocalStorageError(e);
    }
}
