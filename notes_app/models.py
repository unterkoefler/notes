import uuid
from django.db import models

class Note(models.Model):
  id = models.UUIDField(primary_key=True, default=uuid.uuid4, editable=False)
  title = models.TextField()
  contents = models.TextField(blank=True)
  last_updated = models.PositiveBigIntegerField()

  def __str__(self):
      return self.title

  def to_dict(self):
      return {
              "id": self.id,
              "title": self.title,
              "contents": self.contents
              }
