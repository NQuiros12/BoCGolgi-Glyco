#In this code I going to try to make an easy
# #code to administrate your economic
#poner que busque el precio del dolar
#poner que te pregunte cuanto queres ahorrar al mes

class Economy():
    def __init__(self,salary,dollar,saving,debt):
        self.salary=salary
        self.dollar=dollar
        self.saving=saving
        self.debt=debt
        
       
    def savings(self):
        self.saving=int(self.salary*0.1)
        print("Este mes ahorraste "+str(self.saving))
    def debts(self):
        if self.debt==0:
          print("No, tenes deudas este mes.")
        else:
            print("Tenes una deuda de "+ str(self.debt) +" este mes.")
    def convert_currency(self):
        saving_dollarize=((self.saving)/self.dollar)
        print("Tus ahorros en dolares este fueron "+str(saving_dollarize))
print("\n Este programa calcula cuanto podes ahorrar por salario. Ahorrando un 10% del salario.")
print("\nSi no tenes deudas pone cero.")
nico_economy=Economy(int(input('salario: ')),int(input("dolar: ")),int(input("deudas: ")))
nico_economy.savings()
nico_economy.convert_currency()
nico_economy.debts()
#VERSION NUEVA
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


nico_economy= Economy()
nico_economy.savings()
nico_economy.doubts()
print(nico_economy.savings())
