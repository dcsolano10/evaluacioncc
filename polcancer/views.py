from django.http import HttpResponse


def index(request):
    return HttpResponse("Cervical cancer policy evaluation index")