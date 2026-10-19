import json
import re
import sys


def transform_pass(text: str) -> tuple[str, bool]:
    lines = text.splitlines()
    result = []
    changed = False
    i = 0

    while i < len(lines):
        match = re.match(r"^>\s*\[!(DEFINITION|THEOREM|PROOF)\]\s*$", lines[i])

        if not match:
            result.append(lines[i])
            i += 1
            continue
        else:
            changed = True

        kind = match.group(1).lower()
        title = kind.capitalize()
        body = []
        i += 1

        while i < len(lines) and lines[i].startswith(">"):
            body.append(re.sub(r"^>\s?", "", lines[i]))
            i += 1

        result.append(f'<blockquote class="blockquote-tag blockquote-tag-{kind}">')
        result.append(f'<p class="blockquote-tag-title">{title}</p>')
        result.extend(body)
        result.append("</blockquote>")

    return ("\n".join(result), changed)


def transform(text: str) -> str:
    changed = True
    while changed:
        text, changed = transform_pass(text)
    return text


def walk(value):
    if isinstance(value, dict):
        # A chapter's Markdown content lives here.
        chapter = value.get("Chapter")
        if isinstance(chapter, dict):
            content = chapter.get("content")
            if isinstance(content, str):
                chapter["content"] = transform(content)

        # Recursively visit nested chapters and all other structures.
        for child in value.values():
            walk(child)

    elif isinstance(value, list):
        for child in value:
            walk(child)


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "supports":
        sys.exit(0)

    context, book = json.load(sys.stdin)
    walk(book)
    json.dump(book, sys.stdout)
