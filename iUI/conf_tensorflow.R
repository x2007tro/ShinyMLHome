tp_conf_tensorflow <- tabPanel(
  "Tensorflow",
  br(),
  numericInput("ctf_seed", "seed (0 = no control)", value = 0, width = "150px"),
  numericInput("ctf_lr", "learning rate", value = 0.001, width = "150px"),
  numericInput("ctf_bs", "batch size", value = 512, width = "150px")
)