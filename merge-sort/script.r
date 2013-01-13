ggplot(data, aes(x=Size, y=Count)) + scale_x_log2() + geom_point() + geom_smooth(formula=y~x, method=lm)+ xlab('List length') + ylab('Comparison count (log2)') + opts(title='Effect of list length on comparison count in list merge sort')

ggplot(data, aes(x=Shuffle, y=Count, fill=Limit)) + geom_histogram(position='dodge') + geom_errorbar(aes(y=Count, ymin=Count-SD, ymax=Count+SD), position='dodge') + xlab('Shuffling level') + ylab('Comparison count scaling (log2)') + opts(title='Effect of shuffling and quick merge cutoff on comparison count in list merge sort') + scale_fill_discrete(name='Merge cutoff')

data[,'Scatter'] <- factor(data[,'Scatter'], c('None', 10, 500, 'Full'))
ggplot(data, aes(x=Size, y=Cycles, color=Scatter)) + scale_x_log2() + geom_point() + geom_smooth(formula=y~x, method=lm) + xlab('List length') + ylab('Cycles (log2)') + opts(title='Effect of list length and cell scattering on runtime of generic list merge sort') + scale_color_discrete(name='Scattering')

data[,'Shuffle'] <- factor(data[,'Shuffle'], c('None', 1, 2, 5, 10, 50, 100, 500, 1000, 'Full'))
data[,'Limit'] <- factor(data[,'Limit'], c(8, 16, 32, 'Never'))
ggplot(data, aes(x=Shuffle, y=Cycles, fill=Limit)) + geom_histogram(position='dodge') + geom_errorbar(aes(y=Cycles, ymin=Cycles-SD, ymax=Cycles+SD), position='dodge') + xlab('Shuffling level') + ylab('Runtime scaling (log2 cycles)') + opts(title='Effect of shuffling and quick merge cutoff on runtime of generic list merge sort') + scale_fill_discrete(name='Merge cutoff')

data[,'Tweak'] <- factor(factor(data[,'Tweak'], c('FxFxF', 'FxFxT', 'FxTxF', 'FxTxT', 'CxFxF', 'CxFxT', 'CxTxF', 'CxTxT', 'TxFxF', 'TxFxT', 'TxTxF', 'TxTxT')))
ggplot(data, aes(x=Tweak, y=Cycles)) + geom_histogram() + geom_errorbar(aes(y=Cycles, ymin=Cycles-SD, ymax=Cycles+SD)) + ylab('Runtime scaling (log2 cycles)') + xlab('Algorithm tweak (Leaf sort, Cached key, Branchful merge)') + opts(title='Effect of algorithmic tweaks on runtime of generic list merge sort')

data[,'Scatter'] <- factor(data[,'Scatter'], c('None', 10, 500, 'Full'))
ggplot(data, aes(x=Size, y=Cycles, color=Scatter)) + scale_x_log2() + geom_point() + geom_smooth(formula=y~x, method=lm) + xlab('List length') + ylab('Cycles (log2)') + opts(title='Effect of list length and cell scattering on runtime of specialised list merge sort') + scale_color_discrete(name='Scattering')

data[,'Shuffle'] <- factor(data[,'Shuffle'], c('None', 1, 2, 5, 10, 50, 100, 500, 1000, 'Full'))
data[,'Limit'] <- factor(data[,'Limit'], c(8, 16, 32, 'Never'))
ggplot(data, aes(x=Shuffle, y=Cycles, fill=Limit)) + geom_histogram(position='dodge') + geom_errorbar(aes(y=Cycles, ymin=Cycles-SD, ymax=Cycles+SD), position='dodge') + xlab('Shuffling level') + ylab('Runtime scaling (log2 cycles)') + opts(title='Effect of shuffling and quick merge cutoff on runtime of specialised list merge sort') + scale_fill_discrete(name='Merge cutoff')

data[,'Tweak'] <- factor(factor(data[,'Tweak'], c('FxFxF', 'FxFxT', 'FxTxF', 'FxTxT', 'CxFxF', 'CxFxT', 'CxTxF', 'CxTxT', 'TxFxF', 'TxFxT', 'TxTxF', 'TxTxT')))
ggplot(data, aes(x=Tweak, y=Cycles)) + geom_histogram() + geom_errorbar(aes(y=Cycles, ymin=Cycles-SD, ymax=Cycles+SD)) + ylab('Runtime scaling (log2 cycles)') + xlab('Algorithm tweak (Leaf sort, Cached key, Branchful merge)') + opts(title='Effect of algorithmic tweaks on runtime of specialised list merge sort')
