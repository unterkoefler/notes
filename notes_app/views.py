import json
import time
from django.http import JsonResponse
from django.views.decorators.csrf import csrf_exempt
from .models import Note

@csrf_exempt
def sync(request):
    # TODO: check that its a POST
    data = json.loads(request.body)
    notes_with_conflicts = add_or_update_notes(data["notes"])
    notes_with_conflicts += delete_notes(data["deletedNotes"])

    all_notes = Note.objects.all()
    return JsonResponse({
        "notes": [ note.to_dict() for note in all_notes ],
        "conflicts": notes_with_conflicts
    })

def add_or_update_notes(notes):
    notes_with_conflicts = []
    for note in notes:
        if note["lastSynced"] is None:
            add_new_note(note)
        else:
            existing_note = Note.objects.get(pk=note["id"])
            conflicting_id = update_existing_note(note, existing_note)
            if conflicting_id is not None:
                notes_with_conflicts.append(conflicting_id)
    return notes_with_conflicts


def add_new_note(note):
    newNote = Note(
            id=note["id"],
            title=note["title"],
            contents=note["contents"],
            last_updated=int(time.time()) # TODO: move to default?
            )
    newNote.save()


def update_existing_note(note, existing_note):
    if existing_note.last_updated < note["lastSynced"]:
        existing_note.title = note["title"]
        existing_note.contents = note["contents"]
        existing_note.last_updated = int(time.time())
        existing_note.save()
        return None
    elif is_note_unchanged(note, existing_note):
        return None
    else:
        copiedNote = Note(
            title="Copy of: " + note["title"],
            contents=note["contents"],
            last_updated=int(time.time())
        )
        copiedNote.save()
        return copiedNote.id

def is_note_unchanged(note, existing_note):
    return (existing_note.title == note["title"] and
            existing_note.contents == note["contents"])


def delete_notes(notes_to_delete):
    conflicting_notes = []
    for note in notes_to_delete:
        existing_note = Note.objects.get(pk=note["id"])
        if existing_note.last_updated < note["lastSynced"]:
            n.delete()
        else:
            conflicting_notes.append(existing_note.id)
    return conflicting_notes
