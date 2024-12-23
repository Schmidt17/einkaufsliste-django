import uuid
import os

from django.http import HttpResponse, JsonResponse, HttpResponseForbidden
from django.template import loader


BASE_URL = os.environ.get('BASE_URL', 'http://127.0.0.1:8000')


def main(request):
    template = loader.get_template('index.html')

    user_key = request.GET.get('k')

    client_id = uuid.uuid4()

    mqtt_topic_done_updates = f'einkaufsliste/{user_key_part(user_key)}/doneUpdates'
    mqtt_topic_new_item = f'einkaufsliste/{user_key_part(user_key)}/newItem'
    mqtt_topic_item_deleted = f'einkaufsliste/{user_key_part(user_key)}/itemDeleted'
    mqtt_topic_item_updated = f'einkaufsliste/{user_key_part(user_key)}/itemUpdated'

    return HttpResponse(template.render(context={
        'api_key': user_key,
        'client_id': client_id,
        'mqtt_topic_done_updates': mqtt_topic_done_updates,
        'mqtt_topic_new_item': mqtt_topic_new_item,
        'mqtt_topic_item_deleted': mqtt_topic_item_deleted,
        'mqtt_topic_item_updated': mqtt_topic_item_updated,
    }))


def appcache(request):
    template = loader.get_template('main.appcache')

    return  HttpResponse(template.render(), content_type="text/cache-manifest")


def webmanifest(request):
    user_key = request.GET.get('k')

    template = loader.get_template('site.webmanifest')

    return  HttpResponse(template.render(context={
        'start_url': f'{BASE_URL}?k={user_key}'
    }))


def service_worker(request):
    user_key = request.GET.get('k')

    if user_key is None:
        return HttpResponseForbidden()

    template = loader.get_template('service-worker.js')

    return HttpResponse(
        template.render(context={
            'api_key': user_key,
        }),
        content_type="application/javascript",
    )


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


def user_key_part(user_key):
    """Extract a small part of the user key that can be safely sent to the MQTT broker (e.g. as topic prefix)"""
    # return the first 8 characters of the key, if it is at least 16 characters long
    if len(user_key) < 16:
        raise ValueError("User key is too short, has to be at least 16 characters")

    return user_key[:8]