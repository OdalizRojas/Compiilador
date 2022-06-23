import basic

while True:
		text = input('basic > ')
		result, error = basic.ejecutar('<stdin>', text)

		if error: print(error.as_string())
		else: print(result)
