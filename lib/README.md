# Project: Words 4 Music

### Code lib Folder

The lib directory contains various files with function definitions (but only function definitions - no code that actually runs).

The read_h5_file.R is used for read the train data(the 2350 songs feature and fixed the feature to a standard length,where the length is calculated by check the length distribution.

The clusting.R is used for cluster the train data and calculate the P(word|cluster) probability matrix for every feature , and I do check and calculate the predictive rank sum using the test data which I assigned from the original data.

The process_and_calculate_rank.R is used for new songs.

