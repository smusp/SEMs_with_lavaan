
### Data for Tables 21.1, 21.2, 21.3, 21.4 ###

## Get the data
path = here("Green_2023", "data", "satisfactionI.csv") 
df = read.csv(path, header = TRUE)
head(df)

## x - Coping Strategy (a - No strategy; b - Discussion; c - Exercise)
## g - Gender
## c - before/after 
## y - dependent variable (Life Satisfaction)


## Reshape data - long to wide
tab = 0.5 * table(df$x)  # in each condition
df$id = c(rep(1:tab[1], 2), rep(1:tab[2], 2), rep(1:tab[3], 2))  # id variable 

df = reshape(df, timevar = "c", idvar = c("id", "x", "g"), varying = c("pre", "y"), 
   direction = "wide")


## Grand mean center "pre" - the before scores
df$preC = scale(df$pre, scale = FALSE)


## Dummy variables for "Coping Startegy"
df$x1 = ifelse(df$x == "a", 1, 0)
df$x2 = ifelse(df$x == "b", 1, 0)
df$x3 = ifelse(df$x == "c", 1, 0)


## Drop the id variable
df = df[, -3]


## Gender X Coping Strategy interaction
df$sg = interaction(df$x, df$g, sep = "")


## Dummy variables for interaction
dummies = model.matrix(~df$sg - 1)
df = data.frame(df, dummies)
names(df) = gsub("df.sg", "", names(df))




my = mean(df$y)
ma = mean(df[df$s == 'a',]$y)
mb = mean(df[df$s == 'b',]$y)
mc = mean(df[df$s == 'c',]$y)


