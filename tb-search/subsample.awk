BEGIN {srand()}

{
        if (i++ == 0) {
                print $0
        } else if (rand() < 0.01) {
                print $0
        }
}
