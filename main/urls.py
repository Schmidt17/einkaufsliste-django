from django.urls import path

from . import views


urlpatterns = [
    path('', views.main),
    path('items/', views.get_items)
]
