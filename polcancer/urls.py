from django.urls import path

from . import views

app_name='polcancer'
urlpatterns = [
    path('', views.index, name='index'),
    path('vaccination', views.vaccination, name='vaccination'),
    path('population', views.population, name='population')
]