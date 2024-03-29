#!/bin/bash

if [[ -n "$DISPLAY" ]]; then
  gmail=$(find "$HOME/mail/gmail/Inbox/new" -type f | wc -l)
  if [[ "$gmail" -ne 0 ]]; then
    dunstify -i emblem-mail "New Emails" \
      "You have $gmail unread messages in your gmail inbox"
  fi

  posteo=$(find "$HOME/mail/posteo/Inbox/new" -type f | wc -l)
  if [[ "$posteo" -ne 0 ]]; then
    dunstify -i emblem-mail "New Emails" \
      "You have $posteo unread messages in your posteo inbox"
  fi

  workspace=$(find "$HOME/mail/workspace/Inbox/new" -type | wc -l)
  if [[ "$workspace" -ne 0 ]]; then
    dunstify -i emblem-mail "New Emails" \
      "You have $workspace unread messages in your workspace inbox"
  fi
fi
