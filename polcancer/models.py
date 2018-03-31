from django.db import models

# Create your models here.
class Genero(models.Model):
	cod_genero = models.CharField(max_length=1)
	nombre_genero = models.CharField(max_length=50)

	def __str__(self):
		return self.nombre_genero

class CompartimentoBiologico(models.Model):
	nombre = models.CharField(max_length=50)
	codigo = models.CharField(max_length=5)

	def __str__(self):
		return self.nombre

class CompartimentoEdad(models.Model):
	codigo = models.CharField(max_length=6)
	edad_inicio = models.IntegerField(default=0)
	edad_fin = models.IntegerField(default=0)
	paso = models.IntegerField(default=0)

	def __str__(self):
		return self.codigo

class TasaMortalidad(models.Model):
	genero = models.ForeignKey(Genero, on_delete=models.PROTECT)
	tasas = models.TextField(null=True)
	compartimento_edad = models.ForeignKey(CompartimentoEdad, on_delete=models.PROTECT)
	compartimento_biologico = models.ForeignKey(CompartimentoBiologico, on_delete=models.PROTECT)

class MortalidadCBiologico(models.Model):
	compartimento_biologico = models.ForeignKey(CompartimentoBiologico, on_delete=models.PROTECT)
	tasas_mortalidad = models.TextField(null=True)
	codigos = models.TextField(null=True)
	genero = models.ForeignKey(Genero, on_delete=models.PROTECT)
	probabilidad_infeccion = models.TextField(null=True)

class TasasDeFertilidad(models.Model):
	genero = models.ForeignKey(Genero, on_delete=models.PROTECT)
	compartimento_edad = models.ForeignKey(CompartimentoEdad, on_delete=models.PROTECT)
	compartimento_biologico = models.ForeignKey(CompartimentoBiologico, on_delete=models.PROTECT)
	tasas_fertilidad = models.TextField(null=True)

class ParejasSexuales(models.Model):
	genero = models.ForeignKey(Genero, on_delete=models.PROTECT)
	compartimento_edad = models.ForeignKey(CompartimentoEdad, on_delete=models.PROTECT)
	promedio_parejas = models.DecimalField(max_digits=9, decimal_places=6)

class ProbabilidadesBiologicas(models.Model):
	compartimento_edad = models.ForeignKey(CompartimentoEdad, on_delete=models.PROTECT)
	compartimento_biologico = models.ForeignKey(CompartimentoBiologico, on_delete=models.PROTECT)
	probabilidad = models.DecimalField(max_digits=9, decimal_places=6)

class Cobertura(models.Model):
	compartimento_edad = models.ForeignKey(CompartimentoEdad, on_delete=models.PROTECT)
	genero = models.ForeignKey(Genero, on_delete=models.PROTECT)
	cobertura = models.DecimalField(max_digits=15, decimal_places=6)

class Vacunacion(models.Model):
	cobertura = models.BigIntegerField(default=0)
	periodo_inicio = models.IntegerField(default=0)
	periodo_fin = models.IntegerField(default=0)

class Tamizaje(models.Model):
	prueba_primaria = models.TextField(null=True)
	prueba_followup =  models.TextField(null=True)
	periodo_inicio = models.IntegerField(default=0)
	periodo_fin = models.IntegerField(default=0)
	adherencia = models.DecimalField(max_digits=9, decimal_places=6)

class Politica(models.Model):
	esquema_vacunacion = models.ForeignKey(Vacunacion, on_delete=models.PROTECT)
	esquema_tamizaje = models.ForeignKey(Tamizaje, on_delete=models.PROTECT)

class Modelo(models.Model):
	tamano_poblacion = models.BigIntegerField(default=0)
	compartimentos_biologicos = models.TextField(null=False)
	compartimentos_edad = models.TextField(null=False)
	politica = models.ForeignKey(Politica, on_delete=models.PROTECT)








