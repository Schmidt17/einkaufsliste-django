{% load static %}

const addResourcesToCache = async (resources) => {
  const cache = await caches.open("v1");
  await cache.addAll(resources);
};

self.addEventListener("install", (event) => {
  console.log(event);

  event.waitUntil(
    addResourcesToCache([
      "{% static 'materialize.min.css' %}"
    ]),
  );
});
