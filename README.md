# Cryptocurrency market depth (*under development*)

## What?

Measure the liquidity (market depth) of all cryptocurrencies using the measure "*how much can I buy/sell of a given cryptocurrency while only moving the price **x**%*?" (where **x** is currently set to *5*). The more liquid a cryptocurrency is, the larger the quantity you can purchase/sell while only moving the price very little.

## How?

Build a weighted directed graph, where the nodes are (crypto)currencies (e.g. "USD", "BTC", "ETH", etc.) and the edge from e.g. "USD" to "BTC" constitutes the *sell orders*/*asks* of a BTC/USD orderbook (because the sell orders of a BTC/USD orderbook are consumed in order to move from "USD" to "BTC"). Conversely, the edge from "BTC" to "USD" constitutes the *buy orders*/*bids* of a BTC/USD orderbook (again, because the buy orders in a BTC/USD orderbook are consumed in order to move from "BTC" to "USD"). The edge weight is the inverse (*1/n*) of the amount *n* that you can sell/buy into the given orderbook while only moving the price at most **x**% (currently *x=5*).

By 

1. Saving the result from applying Dijkstra's algorithm with source vertex "USD" and target vertex "*crypto_currency*" (to find the most liquid path from "USD" to the given cryptocurrency)
2. Removing the edge(s) returned by 1
3. Repeating

we discover, in descending order of liquidity, all the paths from a given currency to "USD". We repeat this for all cryptocurrencies, in order to find how much one can sell/buy into a given cryptocurrency while only moving the price the specified percentage (using all available exchanges and markets).
