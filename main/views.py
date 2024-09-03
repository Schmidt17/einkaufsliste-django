from django.http import HttpResponse
from django.template import loader


def main(request):
    template = loader.get_template('index.html')

    api_key = request.GET.get('k')

    return HttpResponse(template.render(context={'api_key': api_key}))


def get_items(request):
    return HttpResponse("Milch,Brot,Käse")
