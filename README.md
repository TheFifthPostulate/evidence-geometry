# evidence-geometry
An interpretable evidence-based risk modeling framework for classification problems that:

1. Transforms heterogeneous features into unified log-likelihood ratio (evidence) space

2. Learns class-conditional geometry

3. Provides decomposed interpretable risk signals:
   i. Distance contrast : Difference of Mahalanobis distances relative to learned class manifolds in evidence space  
   ii. Drift projection :  Projection of evidence onto learned mean deviation separation direction between class manifolds  
   iii. Principal component bundle energy : Sum of squares of projections of evidence onto Principal Component directions of positive-class subspace in evidence space  

These risk signals expose distinct modes of risk of each case relative to negative-class likelihood manifold.
In clinically-relevant datasets such as BCW and UCI Heart Disease Cleveland, the risk signals support triage based on reasoning due to ability to directly interpret values, supporting the performance of baseline discriminative classifiers, and reducing False Negative Rate while maintaining a good automation rate.

This repo contains fully executable R notebooks that demonstrate both test cases.
