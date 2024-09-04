from django.http import HttpResponse, JsonResponse
from django.template import loader


def main(request):
    template = loader.get_template('index.html')

    api_key = request.GET.get('k')

    return HttpResponse(template.render(context={'api_key': api_key}))


def get_items(request):
    test_data = [
        {
            "id": "123",
            "title": "Milch",
            "tags": ["Supermarkt"],
            "done": 0
        },
        {
            "id": "456",
            "title": "Shampoo",
            "tags": ["Supermarkt", "Drogerie"],
            "done": 0
        }
    ]

    return JsonResponse(test_data, safe=False)
