
library(dagitty)

library(ggdag)
#Set theme
theme_set(theme_dag())
#Draw the dag for 2022 crosssectional
mod<-dagify(pc~uc,
            pc~f+uc,
            f~uc,
            n~uc,
            pc~n,exposure=c("uc"), outcome="pc", coords=coords)
#define coordinates
coords<-list(
  x=c(pc=1, uc=-1, n=0, f=0),
  y=c(pc=0.5, uc=0.5, n=1, f=0)
)
ggdag(mod)+geom_text(label="n=Northern Ontario\nuc=University Closure\npc=PC Vote Share\nf=Francophones", aes(x=0.75, y=0.9))

#Try to draw the dag 

