# Streams And Parallelism

When working with Streams there are two things one has to take
into account, namely, the granularity of the data to be parallelized, and
buffering the data in order not to destroy the lazy streaming.
There are two general approaches for parallelizing Streams: 1) Strategies with
the Eval Monad or 2) DataFlow Parallelism with the Par Monad.
This example is taken from Parallel And Concurrent Haskell and shows both
approaches in the optimization of the RSA algorithm for encryption and
decryption.

Also note that in this example I'm implementing the suggested Rate-Limiting
for the Producers in the pipeline. This is a nice utility when the producers
are way faster than the consumers. Thus rating their stream-production avoids
having a great overhead in the heap due to garbage collection.