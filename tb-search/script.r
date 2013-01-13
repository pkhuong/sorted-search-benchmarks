sample.data.frame <-
  function(data, fraction=.1)
  {
    n <- length(data[,1])
    data[sample.int(n,n*fraction),]
  }

select.methods <-
  function(data, methods)
  {
    data <- data[is.element(data$method, methods),]
    data$method <- factor(data$method)
    data
  }

low_decile <-
  function (x)
  {
    quantile(x, .1)
  }

high_decile <-
  function (x)
  {
    quantile(x, .9)
  }

ggplot(first.data, aes(x=log2(size), y=count, colour=type)) + scale_colour_discrete(name='Cache level') + stat_summary(fun.y='mean', fun.ymin='min', fun.ymax='max', geom='pointrange') + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Miss per search (average count)') + opts(title='Cache misses for repeated searches in sorted vectors')


ggplot(data, aes(x=log2(size), y=cycles)) + geom_jitter(alpha=1/500, position=position_jitter(height=10), size=.5) + stat_summary(fun.y='median', geom='line', color='green') + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 1500)) + opts(title='Runtimes for repeated searches in sorted vectors')


ggplot(first.cache.bin, aes(x=log2(size), y=count, color=type)) + stat_summary(fun.y = 'median', fun.ymin='min', fun.ymax='max', geom='pointrange') + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Average number of misses (min/median/max)') + opts(title='Cache misses for repeated binary searches in sorted vectors')  + scale_colour_discrete(name='Cache level')

ggplot(first.bin, aes(x=log2(size), y=cycles)) + geom_jitter(alpha=1/500, position=position_jitter(height=10), size=.5) + stat_summary(fun.y='median', geom='line', color='green') + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 1500)) + opts(title='Runtimes for repeated binary searches in sorted vectors')

ggplot(first.prelim, aes(x=log2(size), y=cycles, color=method)) + geom_point(y=2000) + geom_jitter(alpha=1/500, position=position_jitter(height=15), size=1) + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 1500)) + opts(title='Runtimes for repeated searches in sorted vectors')

ggplot(cache.first, aes(x=log2(size), y=count, color=type)) + stat_summary(fun.y = 'median', fun.ymin='min', fun.ymax='max', geom='pointrange') + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Average number of misses (min/median/max)') + opts(title='Cache misses for repeated searches in sorted vectors')  + scale_colour_discrete(name='Cache level') + facet_grid(. ~ method)

ggplot(random.prelim, aes(x=log2(size), y=cycles, color=method)) + geom_point(y=-10000) + geom_jitter(alpha=1/500, position=position_jitter(height=15), size=1) + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 8000)) + opts(title='Runtimes for random searches in sorted vectors')

ggplot(cache.first, aes(x=log2(size), y=count, color=type)) + stat_summary(fun.y = 'median', fun.ymin='min', fun.ymax='max', geom='pointrange') + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Average number of misses (min/median/max)') + opts(title='Cache misses for random searches in sorted vectors')  + scale_colour_discrete(name='Cache level') + facet_grid(. ~ method)


ggplot(random.real, aes(x=log2(size), y=cycles, color=method)) + stat_summary(fun.y = 'median', fun.ymin='low_decile', fun.ymax='high_decile', geom='pointrange') + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 8000)) + opts(title='Runtimes for random searches in sorted vectors')

ggplot(random.real, aes(x=log2(size), y=cycles, color=method)) + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 5500)) + opts(title='Runtimes for random searches in sorted vectors') + stat_summary(fun.y=median, fun.ymin=low_decile, fun.ymax=high_decile, geom="smooth", aes(fill=method))


random256k.real <- select.methods(sample.data.frame(read.table('table-random-256k', header=TRUE)), c('goog', 'stl', 'ob', 'oq', 'tb', 'ter'))

ggplot(random64k.real, aes(x=log2(size), y=cycles, color=method)) + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 1000)) + opts(title='Runtimes for random searches in sorted vectors') + stat_summary(fun.y=median, fun.ymin=low_decile, fun.ymax=high_decile, geom="smooth", aes(fill=method), alpha=.25)

ggplot(first.real, aes(x=log2(size), y=cycles, color=method)) + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 2000), breaks=c(0, 50, 100, 150, 200, 250, 300)) + opts(title='Runtimes for repeated searches in sets') + stat_summary(fun.y=median, fun.ymin=low_decile, fun.ymax=high_decile, geom="smooth", aes(fill=method), alpha=.25) + coord_cartesian(ylim=c(0, 250))

ggplot(random.real, aes(x=log2(size), y=cycles, color=method)) + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 10000), breaks=c(0, 200, 400, 600, 800, 1000)) + opts(title='Runtimes for random searches in sets') + stat_summary(fun.y=median, fun.ymin=low_decile, fun.ymax=high_decile, geom="smooth", aes(fill=method), alpha=.25) + coord_cartesian(ylim=c(0, 1000))

random.small <- select.methods(sample.data.frame(read.table('table-random-small', header=TRUE)), c('goog', 'stl', 'oq', 'ter'))
random.small$param <- factor(random.small$param, c('1K', '16K', '64K'))

ggplot(random.small, aes(x=log2(size), y=cycles, color=method)) + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 10000), breaks=c(0, 50, 100, 150, 200, 250, 300)) + opts(title='Runtimes for random searches in small contiguous subranges of sets') + stat_summary(fun.y=median, fun.ymin=low_decile, fun.ymax=high_decile, geom="smooth", aes(fill=method), alpha=.25) + coord_cartesian(ylim=c(0, 300)) + facet_grid(. ~ param)

ggplot(stride.small, aes(x=log2(size), y=cycles, color=method)) + scale_x_continuous(name='Size of vector (lg(n))') + scale_y_continuous(name='Cycles (f = 2.8GHz)', limits=c(0, 1500), breaks=c(0, 100, 200, 300, 400)) + opts(title='Runtimes for searches in sets with short strides') + stat_summary(fun.y=median, fun.ymin=low_decile, fun.ymax=high_decile, geom="smooth", aes(fill=method), alpha=.25) + coord_cartesian(ylim=c(0, 400)) + facet_grid(. ~ param)

stride.small <- select.methods(sample.data.frame(read.table('table-stride-small', header=TRUE)), c('goog', 'stl', 'oq', 'ter'))
stride.small$param <- factor(stride.small$param, c('1', '16', '64'))
levels(stride.small$param) <- list('1'='1', '17'='16', '65' = '64')
