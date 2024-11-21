{% load static %}

const CURRENT_STATIC_CACHE = 'static-v7';
const CURRENT_DYNAMIC_CACHE = 'dynamic-v7';

const channel = new BroadcastChannel('sw-messages');

const addResourcesToCache = async (resources) => {
  const cache = await caches.open(CURRENT_STATIC_CACHE);
  await cache.addAll(resources);
};

self.addEventListener("install", (event) => {
  self.skipWaiting();

  event.waitUntil(
    addResourcesToCache([
      "/?k={{ api_key }}",

      // images
      "{% static 'img/apple-touch-icon.png' %}",
      "{% static 'img/favicon-32x32.png' %}",
      "{% static 'img/favicon-16x16.png' %}",
      "site.webmanifest?k={{ api_key }}",

      // CSS
      "https://fonts.googleapis.com/icon?family=Material+Icons",
      "{% static 'materialize.min.css' %}",
      "{% static 'main.css' %}",

      // JS
      "{% static 'materialize.min.js' %}",
      "{% static 'CustomChips.js' %}",
      "{% static 'CustomModal.js' %}",
      "{% static 'paho_mqtt.min.js' %}",
      "{% static 'main.js' %}",
    ]),
  );
});

self.addEventListener("activate", (event) => {

    event.waitUntil(
        caches.keys()
            .then( keyList => {
                return Promise.all(keyList.map( key => {
                    if(key !== CURRENT_STATIC_CACHE && key !== CURRENT_DYNAMIC_CACHE) {
                        console.log('service worker --> old cache removed :', key);
                        return caches.delete(key);
                    }
                }))
            })
    );
    return self.clients.claim();
});

self.addEventListener('fetch', event => {
    // check if request is made by chrome extensions or web page
    // if request is made for web page url must contains http.
    if (!(event.request.url.indexOf('http') === 0)) return; // skip the request. if request is not made with http protocol

    const dont_cache_url_patterns = [
        /.\/items\/sync/g,
        /.\/einkaufsliste-multiuser\/api/g,
        /.\/einkaufs_api/g
    ]

    event.respondWith(
        caches.match(event.request)
            .then( response => {
                if(response) {
                    return response;
                } else {
                    return fetch(event.request)
                        .then( res => {
                            if (dont_cache_url_patterns.some(pattern => event.request.url.match(pattern))) return res;

                            return caches.open(CURRENT_DYNAMIC_CACHE)
                                .then( cache => {
                                    cache.put(event.request.url, res.clone());
                                    return res;
                                })
                        });
                }
            })
    );
});

self.addEventListener('sync', event => {

  channel.postMessage({message: 'Back online'});

});