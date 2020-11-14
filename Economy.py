#In this code I going to try to make an easy
# #code to administrate your economic

class Economy():
    def __init__(self,salary,dollar,debt):
        self.salary=salary
        self.dollar=dollar
        self.saving=0
        self.debt=debt
       
    def savings(self):
        self.saving=int(self.salary*0.10)
        print("Este mes ahorraste "+str(self.saving))
    def debts(self):
        if self.debt==0:
          print("No, tenes deudas este mes.")
        else:
            print("Tenes una deuda de "+ str(self.debt) +" este mes.")
    def convert_currency(self):
        saving_dollarize=((self.saving)/self.dollar)
        print("Tus ahorros en dolares este fueron "+str(saving_dollarize))
print("\n Este programa calcula cuanto podes ahorrar por mes")
print("\nSi no posees deudas pone cero.")
nico_economy=Economy(int(input('salario: ')),int(input("dolar: ")),int(input("deudas: ")))
nico_economy.savings()
nico_economy.convert_currency()
nico_economy.debts()
