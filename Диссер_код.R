dag1 <- glm(formula = as.factor(Q7_trust) ~ age_cohort + Q1leisure +Q2obidience + Q2selfexpression + urban + sufi + salafi + secular + urban_born   + gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"), data = dagi2, x = TRUE)
dag2 <- glm(formula = as.factor(Q8_relatives) ~ + age_cohort  +
              Q1leisure +Q2obidience + Q2selfexpression + urban + sufi + salafi + secular+ urban_born +
              gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"),
            data = dagi2, x = TRUE)
dag3 <- glm(formula = as.factor(Q8_friends) ~ age_cohort  +
              Q1leisure +Q2obidience + Q2selfexpression + urban + sufi + salafi + secular+ urban_born +
              gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"),
            data = dagi2, x = TRUE)
dag4 <- glm(formula = as.factor(Q8_locals) ~ age_cohort  +
              Q1leisure + Q2obidience + Q2selfexpression + urban + sufi + salafi + secular+urban_born   +
              gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"),
            data = dagi2, x = TRUE)
dag5 <- glm(formula = as.factor(Q8_colleagues) ~ age_cohort  +
              Q1leisure +Q2obidience + Q2selfexpression + urban + sufi + salafi + secular+ urban_born   +
              gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"),
            data = dagi2, x = TRUE)
dag6 <- glm(formula = as.factor(Q8_ethnic) ~ age_cohort  +
              Q1leisure +Q2obidience + Q2selfexpression+ urban + sufi + salafi + secular+ urban_born   +
              gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"),
            data = dagi2, x = TRUE)
dag7 <- glm(formula = as.factor(Q8_religion) ~ age_cohort  +
              Q1leisure +Q2obidience + Q2selfexpression+ urban + sufi + salafi + secular+ urban_born   +
              gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"),
            data = dagi2, x = TRUE)
dag8 <- glm(formula = as.factor(Q8_all) ~ age_cohort  +
              Q1leisure + Q2obidience + Q2selfexpression+ urban + sufi + salafi + secular+urban_born   +
              gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"),
            data = dagi2, x = TRUE)
dagdag1 <- coeftest(dag1, vcov. = vcovHC(dag1, type="HC0"))
dagdag2 <- coeftest(dag2, vcov. = vcovHC(dag2, type="HC0"))
dagdag3 <- coeftest(dag3, vcov. = vcovHC(dag3, type="HC0"))
dagdag4 <- coeftest(dag4, vcov. = vcovHC(dag4, type="HC0"))
dagdag5 <- coeftest(dag5, vcov. = vcovHC(dag5, type="HC0"))
dagdag6 <- coeftest(dag6, vcov. = vcovHC(dag6, type="HC0"))
dagdag7 <- coeftest(dag7, vcov. = vcovHC(dag7, type="HC0"))
dagdag8 <- coeftest(dag8, vcov. = vcovHC(dag8, type="HC0"))

sink("out3.doc")
stargazer(dagdag1, dagdag2, dagdag3, dagdag4, dagdag5, dagdag6, dagdag7, dagdag8, type="html")
sink()
file.show("out3.doc")

dag9 <- lm(data=dagi2, Q7_trust ~ Q8_index + Q8_all)
sink("out3.doc")
stargazer(dag9, type="html")
sink()
file.show("out3.doc")

dag10 <- glm(formula = as.factor(Workchoice_bi) ~ Q7_trust + Q8_index + age_cohort  + 
              Q1leisure +Q2obidience + Q2selfexpression + urban + sufi + salafi + secular+ urban_born +
              gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"),
            data = dagi2, x = TRUE)
dag11 <- glm(formula = as.factor(Workchoice2_bi) ~ Q7_trust + Q8_index + age_cohort  + 
              Q1leisure +Q2obidience + Q2selfexpression + urban + sufi + salafi + secular+ urban_born +
              gender + education + Q55_reledu + Q56_parents, family = binomial(link = "probit"),
            data = dagi2, x = TRUE)
dagdag10 <- coeftest(dag10, vcov. = vcovHC(dag10, type="HC0"))
dagdag11 <- coeftest(dag11, vcov. = vcovHC(dag11, type="HC0"))

sink("out3.doc")
stargazer(dagdag10, dagdag11, type="html")
sink()
file.show("out3.doc")
