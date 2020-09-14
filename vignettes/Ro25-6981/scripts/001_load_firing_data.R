
library(mema)

mema::load_firing_data(
  units_fname="raw_data/demo_firing_data.mat",
  treatments_fname="raw_data/demo_firing_data_treatments.csv")

mema::load_firing_data(
  units_fname="raw_data/20190405_15h50m35s_units.mat",
  treatments_fname="raw_data/20190405_15h50m35s_treatments_trimmed.csv")

mema::load_firing_data(
  units_fname="raw_data/20190405_12h28m32s_units.mat",
  treatments_fname="raw_data/20190405_12h28m32s_treatments_trimmed.csv")


