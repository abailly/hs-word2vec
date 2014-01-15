# Word2Vec

This is a partial, probably wrong and comparatively slow Haskell port of the [word2vec](https://code.google.com/p/word2vec/)
freamework for computing the vector space of a text corpus. I have started this development following a
[job post](http://www.haskellers.com/jobs/61): I wanted to know whether or not I was able to develop this piece of software, given
I have not been programming anything related to machine learning since my Master degree and I am really no *data scientist*,
whatever that mean...

# What it does?

Following the rules of Zalora's challenge, this code is separated in two parts:

1. `crawl` downloads all documents corresponding to a given query on [Arxiv](http;//arxiv.org), a repository of scientific
    papers. It also converts all those PDFs into text documents using the [pdftotext](http://www.foolabs.com/xpdf/home.html)
    utility program which must be installed on the host system,
2. `word2vec` builds a vector model from all the `.txt` documents in a given directory, and outputs 3 files: A `model.vec` file
   containing the complete description of the neural network trained from the corpora, a `model.pca`mapping all the words in the
   model in a 2-dimensional space using [Principal Component Analysis](http://www.snl.salk.edu/~shlens/pca.pdf), a `model.svg`
   which is an image containing the 100 most frequent words of the corpus drawn in 2D according to previous mapping.

On my recent MacBook Air, training 390 documents, (2M total words) over 56k different words with 100 dimensions takes
approximatively 20minutes. It looks like the words/sec ratio is around 2000, which is much much lower than the optimized C and
Python version but better than the unoptimized pure Python!

# How it works?

This piece of code is 99% Haskell, the remaining 1% being the pdftotext utility I did not find a replacement for. The Haskell port
is not the most brilliant code I ever wrote nor is it intensively tested, but it does the job (at least, it
[works on my machine](http://www.codinghorror.com/blog/2007/03/the-works-on-my-machine-certification-program.html)). Here is a
short summary of how I implemented it:

* The crawling stuff uses mostly plain [Network.Browser](http://hackage.haskell.org/package/network) package with a bit of
  [http-client-conduit](http://hackage.haskell.org/package/http-client-conduit). The retrieval of the documents' ids is
  sequential but the download of the PDF files is done concurrently using a custom built thread pool communicating through plain
  old `MVar`s. I should have used a more sophisticated concurrent package like [async](http://hackage.haskell.org/package/async)
  but...
* There is some parsing of the retrieved page to be done that is handled by [tagsoup](http://hackage.haskell.org/package/tagsoup)
  to retrieve the documents' ids and navigate between the pages of the query.
* Text extraction relies on pdftotext which is spawned using standard process control.
* I used the [tokenize](http://hackage.haskell.org/package/tokenize) package to tokenize the corpus in proper words, removing
  anything that does not contain only letters.
* Tricky part starts with construction of
  [Huffmann encoding](https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html) of the
  words sorted by frequency. I have used [heap](http://hackage.haskell.org/package/heap) package to build the binary tree after
  inserting `(word,frequency)` tuples and the hashmap from
  [unordered-containers](http://hackage.haskell.org/package/unordered-containers) to map words to their encoding
* The meat of the algorithm is training of a neural network made of an `words x dimensions` input layer and an identically sized
  hidden layer. This practically means that both layers are represented as matrices of doubles and we need to run operations on
  part of these matrices for each word in the corpus. I started trying to use
  [hmatrix](http://hackage.haskell.org/package/hmatrix) to compute directly matrix operations on layers as is done in the
  [python port](https://github.com/piskvorky/gensim/blob/develop/gensim/models/word2vec.py) of word2vec (at least in the
  inefficient training algorithm...). But 99% of the time was spent updating the global matrix using the computations from a
  submatrix. I tried to [profile my code](http://www.haskell.org/ghc/docs/latest/html/users_guide/profiling.html) and started
  sprinkling over strictness annotations but was not able to improve significantly the situation. So I went for a simpler and
  uglier solution: Replicate the C loops using [mutable array](http://hackage.haskell.org/package/array) in the IO monad. Given
  the obfuscated nature of the C code, this was not an easy task but I finally managed to get something working with at least
  acceptable performances (for testing purpose of course...). I toyed with the idea of delving into the
  [repa](http://hackage.haskell.org/package/repa) library but finally gave up due to lack of time.
* A surprisingly difficult task was slicing the input sequence of words into random "windows" uniformely distributed around some
  mean value (10) in order to retrieve the central word of the window (the training word) and the list of words around it. I
  devoted a complete module [Window.hs](Window.hs) to this task and it is the best unit-tested of all modules in this code.
* Speaking of tests, while in my daily work I am a big fan and champion of
  [TDD](https://en.wikipedia.org/wiki/Test-driven_development), I admit I forego any tentative whatsoever to do TDD on this
  project and simply relied on types, the REPL and manual coding to get something working. The few existing tests use
  [doctest](http://hackage.haskell.org/package/doctest) to embed small text-based unit tests in documentation of functions.
* The final part of the challenge needed applying PCA on the trained model to extract a 2D mapping of words and generating a graph
  from this mapping. I used [hmatrix-nipals](http://hackage.haskell.org/package/hmatrix-nipals) to do PCA and
  [Chart](http://hackage.haskell.org/package/Chart) with a [diagrams](http://hackage.haskell.org/package/Chart-diagrams)
  backend. The only complex task in this last step was cleaning the mess with cabal after a `--force-reinstalls` due to some
  mismatch in obscure semigroups and category related packages, which was quite painful until I discovered that `cabal-dev` has
  been integrated in `cabal` as
  [cabal sandbox](http://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes).

# Conclusion

This was fun. And challenging. The most challenging part was trying to understand the training algorithm. The python code was very
helpful as it is thoroughly commented and well written. I would not say the same thing about the C code. YMMV of course...

I learnt a things or two:

* Haskell is really, really a great language. I want and need to do more of it!
* Its eco-system is complex, with lot of half-baked and underdocumented libraries, but one can do anything with it, most of the
  times using beautiful and concise code, sometimes using other forms of beauty...
* Machine learning is really, really fun. While working at [Polyspot](http://www.polyspot.com/en/) I touched a little bit on this
  subject but had not had the opportunity to code anything interesting in this domain. I read
  [Introduction to Information Retrieval](http://nlp.stanford.edu/IR-book/information-retrieval-book.html) to gain a better
  knowledge of the subject but have never applied it on the field.
