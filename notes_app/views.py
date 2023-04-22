from django.http import HttpResponse

def index(request):
    return HttpResponse("Yo what it do")
