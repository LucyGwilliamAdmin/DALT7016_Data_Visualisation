source("data_prep.R")

ggplot(data = sex_data_download, mapping = aes(x = Year, y=Value, colour = Sex))+
  geom_line()

ggplot(data = wp_data_download, mapping = aes(x = Year, y=Value, colour = WorkingPattern))+
  geom_line()

ggplot(data = age_data_download, mapping = aes(x = Year, y=Value, colour = AgeGroup))+
  geom_line()

ggplot(data = wr_data_download, mapping = aes(x = Year, y=Value, colour = WorkRegion))+
  geom_line()

ggplot(data = occ_data_download, mapping = aes(x = Year, y=Value, colour = Occupation))+
  geom_line()

ggplot(data = ind_data_download, mapping = aes(x = Year, y=Value, colour = Industry))+
  geom_line()

test1<-paygap_data %>%
  filter(Units=="Disability pay gap (%)") %>%
  filter_at(vars(names(paygap_data)[!names(paygap_data) %in% append(keep, "WorkingPattern")]), all_vars(.=="All"))%>%
  select(!!unlist(keep), "WorkingPattern")

ggplot(data=test1, mapping = aes(x=WorkingPattern, y=Value))+
  geom_bar(stat="identity")+
  labs(title="Disability pay gap")

test2<-paygap_data %>%
  filter(Units=="Gender pay gap (%)") %>%
  filter_at(vars(names(paygap_data)[!names(paygap_data) %in% append(keep, "WorkingPattern")]), all_vars(.=="All"))%>%
  select(!!unlist(keep), "WorkingPattern")

ggplot(data=test2, mapping = aes(x=WorkingPattern, y=Value))+
  geom_bar(stat="identity")+
  labs(title="Gender pay gap")

test3<-paygap_data %>%
  filter(Units=="Ethnicity pay gap (%)") %>%
  select(!!unlist(keep), "Ethnicity")

ggplot(data=test3, mapping = aes(x=Ethnicity, y=Value))+
  geom_bar(stat="identity")+
  labs(title="Ethnicity pay gap")
