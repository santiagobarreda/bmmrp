


sranef = ranef(model_multinomial, summary = FALSE)$S
a1 = sranef[,,1]
a2 = sranef[,,2]
a3 = sranef[,,3]
a4 = sranef[,,3]*0

mu = (a1+a2+a3+a4)/4

c1 = a1 - mu
c2 = a2 - mu
c3 = a3 - mu
c4 = a4 - mu

c1 = posterior_summary (c1)
c2 = posterior_summary (c2)
c3 = posterior_summary (c3)
c4 = posterior_summary (c4)

par (mfrow =c(4,1), mar = c(.1,4,1.,1))
brmplot (c4)
brmplot (c1)
brmplot (c2)
brmplot (c3)

