library(ggplot2)
library(scales)
library(extrafont)
font_import()
loadfonts(device = "pdf")

source("analysis/data_loading.R")



fig <- ds1 %>%
  dplyr::filter(race_desc_seller %in% c("Black/African American", "White")) %>%
  mutate(race_desc_seller = factor(race_desc_seller, levels = c("White", "Black/African American"))) %>%
  dplyr::filter(buyer_full_entitity_type %in% c("iBuyer", "Non-iBuyer (Personal)")) %>%
  mutate(buyer_full_entitity_type = factor(buyer_full_entitity_type, levels = c("iBuyer", "Non-iBuyer (Personal)"))) %>%
  ggplot(aes(x = race_desc_seller, y = sale_price)) +
  geom_boxplot(aes(fill = buyer_full_entitity_type)) +
  scale_y_log10(
    labels = label_comma(),
    breaks = c(5000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000, 2500000, 5000000),
    limits = c(25000, 2500000)
  ) +
  scale_fill_discrete(limits = c("iBuyer", "Non-iBuyer (Personal)")) +
  guides(colour = guide_legend(reverse = T)) +
  xlab("Race of Home Seller(s)") +
  ylab("Observed Sale Price (Log Scale)") +
  labs(fill = "Home Buyer Type") +
  theme(axis.text.x = element_text(face = "bold", color = "black")) +
  theme(axis.text.y = element_text(face = "bold", color = "black")) +
  theme(text = element_text(size = 9, family = "Times New Roman", face = "bold", color = "black")) +
  theme(legend.text = element_text(size = 7, family = "Times New Roman", face = "bold", color = "black")) +
  theme(legend.title = element_text(size = 8, family = "Times New Roman", face = "bold", color = "black")) +
  theme(legend.background = element_rect(color = "#E7E7EB"))

ggsave(file.path("analysis", "figures", "sale_prices.pdf"),
  plot = fig, width = 4, height = 3, device = "pdf"
)
