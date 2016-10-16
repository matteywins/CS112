data(x)
attach(x)

#Remove NA entries
x <- na.omit(x)

# Assign 
Tr = x$anygirls
X = cbind(x$age, x$white, x$party, x$demvote, x$srvlng)
#X = cbind(x$age, x$white, x$party, x$demvote, x$alabort)
#X = cbind(x$white, x$party, x$demvote, x$srvlng, x$female, x$rgroup)
Y = x$nowtot

# Change nboys from factor to numeric 
x$nboys <-  x$nboys <- as.numeric(as.character(x$nboys))


# Change x$rgroup and x$region from binary to factor
x$rgroup <- factor(x$rgroup)
levels(x$rgroup) <- c("Protestant", "None", "Catholic", "Other Christian", "Other religion")
x$region = factor(x$region)

#
set.seed(12345)
  
#Genetic Matching 

genout <- GenMatch(Tr= Tr, X=X, estimand="ATT", M=1, pop.size=200, max.generations=500, wait.generations=50)

mout <- Match(Y = Y, X = X, Tr = Tr, estimand = "ATT", Weight.matrix = genout)
summary(mout)

mb <- MatchBalance(Tr ~ x$age + x$white + x$party + x$demvote + x$srvlng, match.out = mout, nboots =1000)
#mb <- MatchBalance(Tr ~ x$age + x$white + x$party + x$demvote + x$alabort, match.out = mout, nboots =1000)
#mb <- MatchBalance(Tr ~ x$white + x$party + x$demvote + x$srvlng + x$female +x $rgroup, match.out = mout, nboots =1000)
summary(mb)


