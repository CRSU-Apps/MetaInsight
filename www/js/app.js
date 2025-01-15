// Function to pass cookies as Shiny Input
function getCookies(){
  var res = Cookies.get();
  Shiny.setInputValue('cookies', res);
}

// Once shiny is loaded, call getCookies
// to load cookies as a Shiny input
$(document).on('shiny:connected', function(ev){
  getCookies();
})

// Set a cookie (insecure) and call getCookies
// to update Shiny Input
Shiny.addCustomMessageHandler('cookie-set', function(msg){
  Cookies.set(msg.name, msg.value, { expires: 365});
  getCookies();
})

// remove a cookie and call getCookies
// to update Shiny Input
Shiny.addCustomMessageHandler('cookie-remove', function(msg){
  Cookies.remove(msg.name);
  getCookies();
})
  
// Set a cookie (secure) and call getCookies
// to update Shiny Input
Shiny.addCustomMessageHandler('cookie-set-secure', function(msg){
  Cookies.set(msg.name, msg.value, { expires: 365, sameSite: 'none', secure:true });
  getCookies();
})