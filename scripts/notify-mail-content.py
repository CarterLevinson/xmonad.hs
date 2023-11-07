#!/usr/bin/env python3

from os           import getenv
from sys          import argv, exit
from subprocess   import run
from pathlib      import Path
from email.parser import BytesParser
from email.header import Header, decode_header
from email.errors import HeaderParseError

if getenv("DISPLAY") is None:
    exit()
elif len(argv) != 2:
    print(f"Usage: {argv[0]} maildir")
    exit()
else:
    maildir= Path(argv[1])

for path in maildir.rglob("Inbox/new/*"):
    with open(path, "rb") as handle:
        message = BytesParser().parse(handle)
    try:
        subject = ''.join([
            frag.decode(enc if enc else "utf-8")
                if isinstance(frag, bytes) else frag
                    for frag, enc in decode_header(message['subject'])])
        sender = ''.join([
            frag.decode(enc if enc else "utf-8")
                if isinstance(frag, bytes) else frag
                    for frag, enc in decode_header(message['from'])])
        reciever = ''.join([
            frag.decode(enc if enc else "utf-8")
                if isinstance(frag, bytes) else frag
                    for frag, enc in decode_header(message['to'])])
    except (HeaderParseError, UnicodeDecodeError): # maybe warn about error?
        subject = message['subject']
        if isinstance (subject, bytes):
            subject = subject.decode("utf-8")
        sender = message['sender']
        if isinstance (sender, bytes):
            sender = sender.decode("utf-8")
        if isinstance (reciever, bytes):
            reciever = reciever.decode("utf-8")
    subj = f"Subject: {subject}\n"
    send = f"From: {sender}\n"
    recv = f"To: {reciever}\n"
    run(["notify-send", "-i", "mail-message-new", "Email recieved", subj + send + recv])
