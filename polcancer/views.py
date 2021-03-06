from django.http import HttpResponse
from django.template import loader


def index(request):
	template = loader.get_template('polcancer/inicio.html')
	context = {'gato':'gatito'}
	return HttpResponse(template.render(context, request))

def vaccination(request):
	template = loader.get_template('polcancer/vaccination.html')
	context = {'gato':'gatito'}
	return HttpResponse(template.render(context, request))

def dashboard(request):
	template = loader.get_template('polcancer/charts.html')
	context = {'gato':'gatito'}
	return HttpResponse(template.render(context, request))

def screening(request):
	template = loader.get_template('polcancer/screening.html')
	context = {'gato':'gatito'}
	return HttpResponse(template.render(context, request))

def population(request):
    return HttpResponse("Vamos a parametrizar el modelo de población")
