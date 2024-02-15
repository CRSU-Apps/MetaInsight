// Based on code found at:
// https://github.com/RinteRface/shinyLocalStorage/tree/main
// 
// Copyright (c) 2024 David Granjon
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

function storageAvailable(type) {
  let storage;
  try {
    storage = window[type];
    const x = "__storage_test__";
    storage.setItem(x, x);
    storage.removeItem(x);
    return true;
  } catch (e) {
    return (
      e instanceof DOMException &&
      // everything except Firefox
      (e.code === 22 ||
        // Firefox
        e.code === 1014 ||
        // test name field too, because code might not be present
        // everything except Firefox
        e.name === "QuotaExceededError" ||
        // Firefox
        e.name === "NS_ERROR_DOM_QUOTA_REACHED") &&
      // acknowledge QuotaExceededError only if there's something already stored
      storage &&
      storage.length !== 0
    );
  }
}

if (storageAvailable("localStorage")) {
  // Global variable
  storedVals = {};
  // Retrieve stored data
  $(document).one('shiny:connected', function() {
    if (localStorage.length > 0) {
      for (var i = 0; i < localStorage.length; i ++) {
        var tmpKey = localStorage.key(i);
        storedVals[tmpKey] = localStorage.getItem(tmpKey);
      }
      Shiny.setInputValue('storage', storedVals, {priority: 'event'});
    } else {
      Shiny.setInputValue('storage', null);
    }
  });
  
  // Set stored data
  Shiny.addCustomMessageHandler('add-to-storage', function(m) {
    // update local storage (for persistent effect)
    localStorage.setItem(m.id, m.value);
    
    // also update shiny input (for the current session)
    storedVals[m.id] = m.value;
    Shiny.setInputValue('storage', storedVals, {priority: 'event'});
  });
  
  // Remove stored data
  Shiny.addCustomMessageHandler('remove-from-storage', function(m) {
    // update local storage (for persistent effect)
    localStorage.removeItem(m.id);
    
    // also update shiny input (for the current session)
    delete storedVals[m.id];
    Shiny.setInputValue('storage', storedVals, {priority: 'event'});
  });
} else {
  // Storage is unavailable
  Shiny.setInputValue('storage', null);
}
