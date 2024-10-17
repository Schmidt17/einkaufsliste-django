from django.urls import path

from . import views


urlpatterns = [
    path('', views.main),
    path('main.appcache', views.appcache),
    path('service-worker.js', views.service_worker),
    path('site.webmanifest', views.webmanifest),
    path('items/', views.get_items)
]
