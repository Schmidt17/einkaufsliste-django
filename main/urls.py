from django.urls import path

from . import views


urlpatterns = [
    path('', views.main),
    path('main.appcache', views.appcache),
    path('items/', views.get_items)
]
