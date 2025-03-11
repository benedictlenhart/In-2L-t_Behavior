#combine feeding data
f6 = readRDS("exp6feeding")
f7 = readRDS("exp7feeding")
f10 = readRDS("exp10feeding")

head(f6)

head(f7)

head(f10)
f6.fixed = f6 %>% 
  rename("flyid"="flyid.total") %>% 
  select(-geno) %>% 
  mutate(temp = 25,
         exp = 6) %>% 
  mutate(inv.st = case_when(inv.st == "K1.Homozygous-Inverted" ~ "K3.Inverted",
                            inv.st == "K3.Homozygous.Standard" ~ "K1.Standard",
                            T ~ "K2.Heterozygous"))
f7fixed = f7 %>% 
  mutate(sex = "female",
         exp = 7)%>% 
  mutate(inv.st = case_when(inv.st == "I" ~ "K3.Inverted",
                            inv.st == "H" ~ "K1.Standard",
                            T ~ "K2.Heterozygous"))
f10fixed = f10 %>% 
  mutate(sex = "female",
         exp = 10)%>% 
  mutate(inv.st = case_when(inv.st == "K2.I" ~ "K3.Inverted",
                            inv.st == "K1.S" ~ "K1.Standard",
                            T ~ "K2.Heterozygous"))
fullfeed = rbind(f6.fixed, f7fixed, f10fixed)
saveRDS(fullfeed,"fullfeed")

