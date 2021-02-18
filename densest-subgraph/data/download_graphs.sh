#!/bin/sh

# General Relativity and Quantum Cosmology collaboration network
wget "http://snap.stanford.edu/data/ca-GrQc.txt.gz"
gunzip -df ca-GrQc.txt.gz

# High Energy Physics - Theory collaboration network
wget "http://snap.stanford.edu/data/ca-HepTh.txt.gz"
gunzip -df ca-HepTh.txt.gz

# High Energy Physics - Phenomenology collaboration network
wget "http://snap.stanford.edu/data/ca-HepPh.txt.gz"
gunzip -df ca-HepPh.txt.gz

# Collaboration network of Arxiv Astro Physics
wget "http://snap.stanford.edu/data/ca-AstroPh.txt.gz"
gunzip -df ca-AstroPh.txt.gz

# Collaboration network of Arxiv Condensed Matter
wget "http://snap.stanford.edu/data/ca-CondMat.txt.gz"
gunzip -df ca-CondMat.txt.gz

# Enron email network
wget "http://snap.stanford.edu/data/email-Enron.txt.gz"
gunzip -df email-Enron.txt.gz

# Gowalla
wget "http://snap.stanford.edu/data/loc-gowalla_edges.txt.gz"
gunzip -df loc-gowalla_edges.txt.gz

# Facebook nets
wget "http://snap.stanford.edu/data/facebook_combined.txt.gz"
gunzip -df facebook_combined.txt.gz

# Road net CA
wget "http://snap.stanford.edu/data/roadNet-CA.txt.gz"
gunzip -df roadNet-CA.txt.gz

wget "http://snap.stanford.edu/data/roadNet-PA.txt.gz"
gunzip -df roadNet-PA.txt.gz

wget "http://snap.stanford.edu/data/roadNet-TX.txt.gz"
gunzip -df roadNet-TX.txt.gz

wget "http://snap.stanford.edu/data/loc-brightkite_edges.txt.gz"
gunzip -df loc-brightkite_edges.txt.gz
