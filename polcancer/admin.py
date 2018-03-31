from django.contrib import admin

# Register your models here.
from .models import Genero
from .models import CompartimentoBiologico
from .models import CompartimentoEdad
from .models import TasaMortalidad
from .models import MortalidadCBiologico
from .models import TasasDeFertilidad
from .models import ParejasSexuales
from .models import ProbabilidadesBiologicas
from .models import Cobertura
from .models import Vacunacion
from .models import Tamizaje
from .models import Politica
from .models import Modelo


admin.site.register(Genero)
admin.site.register(CompartimentoBiologico)
admin.site.register(CompartimentoEdad)
admin.site.register(TasaMortalidad)
admin.site.register(MortalidadCBiologico)
admin.site.register(TasasDeFertilidad)
admin.site.register(ParejasSexuales)
admin.site.register(ProbabilidadesBiologicas)
admin.site.register(Cobertura)
admin.site.register(Vacunacion)
admin.site.register(Tamizaje)
admin.site.register(Politica)
admin.site.register(Modelo)

