module Portuguese (
    timeLocalePtBr
) where

import Data.Time.Format

timeLocalePtBr :: TimeLocale
timeLocalePtBr = TimeLocale {
        wDays  = [("Domingo",  "Dom"),  ("Segunda",   "Seg"),
                  ("Terça",    "Ter"),  ("Quarta",    "Qua"),
                  ("Quinta",   "Qui"),  ("Sexta",     "Sex"),
                  ("Sábado",   "Sáb")],

        months = [("Janeiro",   "Jan"), ("Fevereiro", "Fev"),
                  ("Março",     "Mar"), ("Abril",     "Abr"),
                  ("Maio",      "Mai"), ("Junho",     "Jun"),
                  ("Julho",     "Jul"), ("Agosto",    "Ago"),
                  ("Setembro",  "Set"), ("Outubro",   "Out"),
                  ("Novembro",  "Nov"), ("Dezembro",  "Dez")],

        amPm = ("am", "pm"),
        dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
        dateFmt = "%d/%m/%y",
        timeFmt = "%H:%M:%S",
        time12Fmt = "%I:%M:%S %p"
        }

