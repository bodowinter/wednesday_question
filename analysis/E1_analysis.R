## Bodo Winter
## March 22, 2017
## Analysis of Experiment 1, Wednesday Question

## Load packages:

library(tidyverse)
library(stringr)

## Load in data:

setwd('/Users/winterb/Research/wednesday_question/github/data/')
xdata <- read.csv('E1_forwardbackward.csv', skip = 2, stringsAsFactors = F, header = F)
header <- readLines('E1_forwardbackward.csv', n = 1)
colnames(xdata) <- unlist(str_split(header, ','))

## Rename:

xdata <- xdata %>%
	rename(Control = MathQuestion,
		Age = age,
		Gender = gender,
		Handedness = handedness,
		NativeLanguage = language,
		Condition = `DO-BR-FL_10`,
		Comments = OpenEnded,
		Notice = GestureNotice,
		ViewN = VideoNTimes,
		ViewYes = VideoCompletion)

## Get all_responses:

xdata$Response <- NA
these_cols <- str_detect(colnames(xdata),
	'_resp')
timing_cols <- colnames(xdata)[str_detect(colnames(xdata), 'Timing')]
for (i in 1:nrow(xdata)) {
	this <- unlist(xdata[i, these_cols])
	names(this) <- c()
	if (!all(this == '')) {
		xdata[i,]$Response <- this[this != '']
		}
	}

## Get rid of Bodo's response:

xdata <- filter(xdata,
	Comments != 'bodo')

## Exclude those partials that did not respond:

(old.N <- nrow(xdata))
xdata <- filter(xdata, !is.na(Response))
(new.N <- nrow(xdata))
old.N - new.N		# N of responses deleted
(1 - (new.N / old.N)) * 100

## Select subset of dataframe:

xdata <- select(xdata,
	ViewYes:NativeLanguage, Notice, Condition, Response)

## Make into tibble:

xdata <- as_tibble(xdata)

## Get rid of non-native speakers:

(old.N <- nrow(xdata))
excludes <- c('French', '', 'no', 'Vietnamese')
xdata <- filter(xdata,
	!(NativeLanguage %in% excludes))
(new.N <- nrow(xdata))
old.N - new.N		# N of responses deleted
(1 - (new.N / old.N)) * 100

## Get rid of multiple viewers:

table(xdata$ViewYes)		# check
table(xdata$ViewN)

(old.N <- nrow(xdata))
yesses <- c('1', 'once', 'one', 'Once',
	'Only 1, as I thought I only can watch it one according to the instruction.',
	'One')
xdata <- filter(xdata,
	ViewN %in% yesses,
	ViewYes == 'Yes')
(new.N <- nrow(xdata))
old.N - new.N		# N of responses deleted
(1 - (new.N / old.N)) * 100

## Make a Monday/Friday column:

table(xdata$Response)
xdata$RespClean <- NA
xdata <- mutate(xdata,
	Response = str_to_lower(Response))
xdata[str_detect(xdata$Response, 'monday'), ]$RespClean <- 'monday'
xdata[str_detect(xdata$Response, 'friday'), ]$RespClean <- 'friday'

## Check:

table(xdata$RespClean, useNA = 'always')
table(xdata$RespClean, useNA = 'always') / nrow(xdata)
table(xdata$RespClean)
round(prop.table(table(xdata$RespClean)) * 100, 2)
binom.test(table(xdata$RespClean))

## Code condition column:

conds <- separate(xdata[, 'Condition'],
	Condition,
	into = c('experiment', 'Cond', 'Gesture', 'Language'))

## Append:

xdata <- as_tibble(cbind(xdata,
	select(conds, Gesture:Language)))

## Make a column for gesture awareness:

table(xdata$Notice)
xdata$Notice01 <- (-2)
xdata[xdata$Notice == 'Probably not', ]$Notice01 <- (-1)
xdata[xdata$Notice == 'Might or might not', ]$Notice01 <- 0
xdata[xdata$Notice == 'Probably yes', ]$Notice01 <- 1
xdata[xdata$Notice == 'Definitely yes', ]$Notice01 <- 2

## Make a table:

table(xdata$RespClean, xdata$Gesture)
table(xdata$RespClean, xdata$Language)
table(xdata$RespClean, xdata$Language, xdata$Gesture)

## Contrast-code for logistic regression model:

xdata <- mutate(xdata,
	RespClean = as.factor(RespClean),
	Language = as.factor(Language),
	Gesture = as.factor(Gesture))
contrasts(xdata$Language) <- contr.sum(2) / 2
contrasts(xdata$Gesture) <- contr.sum(2) / 2

## Make a logistic regression model with notice:

summary(xmdl.full <- glm(RespClean ~ Language * Gesture + Notice01 + Notice01:Gesture,
	data = xdata, family = 'binomial'))
summary(xmdl.red <- glm(RespClean ~ Language * Gesture,
	data = xdata, family = 'binomial'))
anova(xmdl.red, xmdl.full, test = 'Chisq')	# nope

## Main result to report:

table(xdata$RespClean, xdata$Language)



