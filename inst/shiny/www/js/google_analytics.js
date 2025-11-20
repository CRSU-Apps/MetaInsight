// Import the Google Tag JS
var js = document.createElement("script");
js.type = "text/javascript";
js.src = "https://www.googletagmanager.com/gtag/js?id=<<GOOGLE_ANALYTICS_ID>>";
document.body.appendChild(js);

window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());
gtag('config', '<<GOOGLE_ANALYTICS_ID>>');

// Event Tracking Code
Shiny.addCustomMessageHandler('add-event', function(msg){
  gtag('event', msg.value, {
    'event_value': msg.value
  });
})
