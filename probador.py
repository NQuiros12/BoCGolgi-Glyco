class Economy():
    
    def __init__(self):
        #Salario del usuario en forma de input
        self.salary=float(input("Salario "))
        #Porcentaje que queremos ahorrar
        self.percentage=float(input("Porcentaje que quiere ahorrar "))
        #Si hay deudas para restarlas del sueldo. Asumo que va a pagar.
        self.doubt =float(input("Deudas:\nSino posee deudas coloque cero "))
        #Inicializo las variables
        self.saving = 0
        self.dollar= 153.0#Hay que buscar como hacer que lo busque automatico
        self.savesDollar= 0

    def savings(self):
        #Calcular los ahorros en base al porcentaje brindado por el usuario
        self.saving = self.salary * (self.percentage/100)

        self.savesDollar = round(self.saving / self.dollar,3)
    def doubts(self):
        if self.doubt == 0:
            print("No hay deudas este mes")
        else:
            print("Duedas:",self.doubt)
        print("Este mes ahorraste", self.saving)
        print("Esos ahorros en dolares son",self.savesDollar)


nico_economy = Economy()
nico_economy.savings()
nico_economy.doubts()
print(nico_economy.savings())
