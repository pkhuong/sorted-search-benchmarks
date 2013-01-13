clamp <-
  function(data)
  {
    ddply(data, .(Search, Size),
          function (data)
          {
            limit <- quantile(data[,'Ticks'], .99)
            data[data$Ticks <= limit,]            
          })
  }

dataCache <- new.env(parent=emptyenv())

readData <-
  function (file)
  {
    if (is.data.frame(file)) {
      file
    } else if (exists(file, env=dataCache)) {
      get(file, env=dataCache)
    } else {
      data <- clamp(read.table(file, header=TRUE))
      data$Search = factor(data$Search, c('lin', 'inv', 'vec', 'bin'));
      data$Search = factor(data$Search)
      data$Size = factor(data$Size, c(2, 4, 8, 16, 32, 64, "32*" , "64*"))
      data$Size = factor(data$Size)
      assign(file, data, env=dataCache)
      
      data
    }
  }

boxplots <-
  function (file, title)
  {
    data <- readData(file)
    ggplot(data, aes(x = Search, y = Ticks)) + facet_grid(. ~ Size) + geom_boxplot() + xlab('Search algorithm') + opts(title=title)
  }

histograms <-
  function (file, title)
  {
    data <- readData(file)
    ggplot(data, aes(x = Ticks, fill = Search)) + facet_grid(. ~ Size, scales='free_x') + geom_histogram(aes(y=..density..), position='identity', alpha=.3) + xlab('Latency (ticks)') + opts(title=title) + scale_fill_discrete(name='Search alg.')
  }

scatter_box <-
  function (file, title, outliers=0)
  {
    data <- readData(file)
    summary <- ddply(data, .(Search, Size), summarise, Ticks.mean=mean(Ticks))
    ggplot(data, aes(x = Search, color=Search, y=Ticks)) + facet_grid(. ~ Size) +
      opts(legend.position='none') +
      geom_jitter(alpha=1/225, position=position_jitter(height=1)) +
      geom_crossbar(data=summary, aes(y=Ticks.mean, ymin=Ticks.mean, ymax=Ticks.mean, x=Search),
                   size=.25, colour='grey') +
      geom_boxplot(aes(y=Ticks), color='black',fill='transparent',
                   size = .5, outlier.size=outliers) +
      xlab('Search algorithm') + ylab('Execution time (cycles, f=2.8 GHz)') +
      opts(title=title)
  }

make_graph <- function(img, datafile, title)
  {
    png(img, w=600, h=400, bg='transparent')
    scatter_box(datafile, title)
    dev.off()
  }

make_graphs <-
  function ()
  {
    make_graph('box-random-cached.png', 'bench/search-cache',
               'Latency of random lookups in a cached vector of size …')
    make_graph('box-random-uncached.png', 'bench/search-uncached',
               'Latency of random lookups in uncached vectors of size …')
    make_graph('box-fixed-cached.png', 'bench/no-search-cache',
               'Latency of fixed lookups in a cached vector of size …')
    make_graph('box-fixed-uncached.png', 'bench/no-search-uncached',
               'Latency of fixed lookups in uncached vectors of size …')
    make_graph('box-split-cached.png', 'bench/half-search-cache',
               'Latency of low-variation lookups in a cached vector of size …')
    make_graph('box-split-uncached.png', 'bench/half-search-uncached',
               'Latency of low-variation lookups in uncached vectors of size …')
  }

function ()
  {
    dataCache <<- new.env(parent=emptyenv())
    png('box-random-cached.png', w=600, h=400, bg='transparent')
scatter_box('bench/search-cache', 'Execution time of random searches in a cached vector of size …')
dev.off()
png('box-random-uncached.png', w=600, h=400, bg='transparent')
scatter_box('bench/search-uncached', 'Execution time of random searches in uncached vectors of size …')
dev.off()
png('box-fixed-cached.png', w=600, h=400, bg='transparent')
scatter_box('bench/no-search-cache', 'Execution time of fixed searches in a cached vector of size …')
dev.off()
png('box-fixed-uncached.png', w=600, h=400, bg='transparent')
scatter_box('bench/no-search-uncached', 'Execution time of fixed searches in uncached vectors of size …')
dev.off()
png('box-split-cached.png', w=600, h=400, bg='transparent')
scatter_box('bench/half-search-cache', 'Execution time of low-variation searches in a cached vector of size …')
dev.off()
png('box-split-uncached.png', w=600, h=400, bg='transparent')
scatter_box('bench/half-search-uncached', 'Execution time of low-variation searches in uncached vectors of size …')
dev.off()

  }
