set.seed(2015)
N=1E3
genotype_elements = c(-1,0,1)
#genotype_elements = rnorm(N)
error = rnorm(N)
tb = data.frame(error)
tb$g1 = sample(genotype_elements, N, replace=T)
h2 = 0.5
tb$phynotype = sqrt(h2)*tb$g1 + sqrt(1-h2)*tb$error
summary(lm(tb$phynotype ~ tb$g1))

tb$g2 = sample(genotype_elements, N, replace=T)
h2 = 1/10
tb$phynotype = sqrt(h2)*tb$g1 + sqrt(h2)*tb$g2+ sqrt(1-2*h2)*tb$error
summary(lm(tb$phynotype ~ tb$g1 + tb$g2))

h2 = 1/10
tb$g3 = sample(genotype_elements, N, replace=T)
tb$phynotype = sqrt(h2)*tb$g1 + sqrt(h2)*tb$g2 + sqrt(h2)*tb$g3 + sqrt(1-2*h2)*tb$error
summary(lm(tb$phynotype ~ tb$g1 + tb$g2 + tb$g3))
summary(lm(tb$phynotype ~ tb$g1 ))
summary(lm(tb$phynotype ~ tb$g2 ))
summary(lm(tb$phynotype ~ tb$g3 ))




