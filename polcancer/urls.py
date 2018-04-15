from django.urls import path

from . import views

urlpatterns = [
    path('', views.index, name='index'),
    path('population', views.population, name='population')
]