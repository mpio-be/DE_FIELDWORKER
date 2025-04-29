hatching_table <- function() {

  x = fread('float_angle	surface	days_till_hatching
    30	0	25
    35	0	24
    40	0	23
    45	0	22
    50	0	22
    55	0	21
    60	0	21
    65	0	20
    70	0	20
    75	0	20
    80	0	19
    90	0	19
    90	1	16
    90	2	14
    90	3	13
    90	4	11
    90	5	8
  ')


  x1 = data.table(float_angle = 10:90, surface = 0)

  o = merge(x, x1, all = TRUE)

  setnafill(o, "nocb")

  o


}