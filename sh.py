import esperanto

while True:
    text = input('esperanto > ')
    result, error = esperanto.run('<stdin>', text)

    if error: print(error.as_string())
    else: print(result)
