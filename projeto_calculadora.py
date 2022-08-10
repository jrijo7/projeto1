print("------------Calculadora------------")


print("1. Soma")
print("2. Subtração")
print("3. Produto")
print("4. Divisão")

operacao = int(input("Escolha a operação que deseja realizar (1/2/3/4): "))

x = int(input("Digite o primeiro número: "))
y = int(input("Digite o segundo número: "))

if operacao == 1:
	soma = x + y
	print(soma)
elif operacao == 2:
	subtracao = x - y
	print(subtracao)
elif operacao == 3:
	produto = x*y
	print(produto)
elif operacao == 4:
        if y == 0:
                print("Impossível dividir por zero.")
        else:
                divisao = x/y
                print(divisao)
else:
        print("Operação inválida, faça uma conta viável jovem mancebo!")
        