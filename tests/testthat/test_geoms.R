context("Geoms plotting")

d<-eq_clean_data(dataset_noaa_raw)
p<-d%>%subset(COUNTRY=="USA" | COUNTRY=="CHINA")%>%subset(DATE>=as.Date("2000-01-01"))%>%
   ggplot(aes(DATE, COUNTRY, colour = DEATHS, size = EQ_PRIMARY, label = LOCATION_NAME)) +
    geom_timeline()+ geom_timeline_label(n_max=5)+
     guides(size = guide_legend("Richter scale value")) +
     guides(colour = guide_colorbar("# Deaths", label.theme = element_text(angle = 45, size = 8 ))) +
     ylab("") +
     theme_classic() +
     theme(legend.position="bottom", axis.line.y = element_blank())



testthat::test_that("Is geoms produce ggplot object", {
  testthat::expect_that(
    p, is_a("ggplot"))
})
