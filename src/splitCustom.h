/* 
   vvvvvvvv External Constants Below -- Do Not Change vvvvvvvv
*/

#define LEFT      0x01
#define RIGHT     0x02

#define CLAS_FAM     0
#define REGR_FAM     1
#define SURV_FAM     2
#define CRSK_FAM     3

/* 
   ^^^^^^^^ External Constants Above -- Do Not Change ^^^^^^^^
*/


/* 
   vvvvvvvv Do Not Touch These Delarations Below vvvvvvvv
*/

void registerCustomFunctions();

extern void registerThis (double (*func) (unsigned int    n,
                                          char           *membership,
                                          double         *time,
                                          double         *event,

                                          unsigned int    eventTypeSize,
                                          unsigned int    eventTimeSize,
                                          double         *eventTime,

                                          double         *response,
                                          double          mean,
                                          double          variance,
                                          unsigned int    maxLevel,

                                          double        **feature,
                                          unsigned int    featureCount),

                          
                          unsigned int family,
                          unsigned int slot);


/* 
   ^^^^^^^^ Do Not Touch These Delarations Above ^^^^^^^^
*/




/*
   Declare your custom functions below:
*/
double new_raph(int Nplus,int ntotal);


double getCustomSplitZipPoisson (unsigned int n,
                                                      char        *membership,
                                                      double      *time,
                                                      double      *event,

                                                      unsigned int eventTypeSize,
                                                      unsigned int eventTimeSize,
                                                      double      *eventTime,

                                                      double      *response,
                                                      double       mean,
                                                      double       variance,
                                                      unsigned int maxLevel,

                                                      double     **feature,
                                                      unsigned int featureCount);


double log_pnbinom(int q, double mu, double theta,int lower_tail);
double log_likelihood (double lmu, double ltheta, double *data , int n);
double grad_ll_lmu (double lmu, double ltheta, double *data, int n );
double grad_ll_ltheta (double lmu, double ltheta, double *data, int n);
double grad_ll_lmu2 (double lmu, double ltheta, double *data, int n);
double grad_ll_ltheta2 (double lmu, double ltheta, double *data, int n);
double grad_ll_lmultheta (double lmu, double ltheta, double *data, int n);

void newtonRaphson ( double *est_par, double *data, int n);
void newtonRaphson_fast ( double *est_par, double *data, int n);
double getCustomSplitztnb (unsigned int n,
                           char        *membership,
                           double      *time,
                           double      *event,
                           
                           unsigned int eventTypeSize,
                           unsigned int eventTimeSize,
                           double      *eventTime,
                           
                           double      *response,
                           double       mean,
                           double       variance,
                           unsigned int maxLevel,
                           
                           double     **feature,
                           unsigned int featureCount);

double getCustomSplitztnb_fast (unsigned int n,
                                char        *membership,
                                double      *time,
                                double      *event,
                                
                                unsigned int eventTypeSize,
                                unsigned int eventTimeSize,
                                double      *eventTime,
                                
                                double      *response,
                                double       mean,
                                double       variance,
                                unsigned int maxLevel,
                                
                                double     **feature,
                                unsigned int featureCount);

double getCustomSplitStatisticMultivariateRegression  (unsigned int  n,
                                                       char         *membership,
                                                       double       *time,
                                                       double       *event,
                                                       
                                                       unsigned int  eventTypeSize,
                                                       unsigned int  eventTimeSize,
                                                       double       *eventTime,
                                                       
                                                       double       *response,
                                                       double        mean,
                                                       double        variance,
                                                       unsigned int  maxLevel,
                                                       
                                                       double      **feature,
                                                       unsigned int  featureCount);



double getCustomSplitStatisticMultivariateClassification (unsigned int  n,
                                                          char         *membership,
                                                          double       *time,
                                                          double       *event,

                                                          unsigned int  eventTypeSize,
                                                          unsigned int  eventTimeSize,
                                                          double       *eventTime,

                                                          double       *response,
                                                          double        mean,
                                                          double        variance,
                                                          unsigned int  maxLevel,
                                                      
                                                          double      **feature,
                                                          unsigned int  featureCount);


double getCustomSplitStatisticSurvival (unsigned int  n,
                                        char         *membership,
                                        double       *time,
                                        double       *event,

                                        unsigned int  eventTypeSize,
                                        unsigned int  eventTimeSize,
                                        double       *eventTime,

                                        double       *response,
                                        double        mean,
                                        double        variance,
                                        unsigned int  maxLevel,

                                        double      **feature,
                                        unsigned int  featureCount);


double getCustomSplitStatisticCompetingRisk (unsigned int  n,
                                             char         *membership,
                                             double       *time,
                                             double       *event,

                                             unsigned int  eventTypeSize,
                                             unsigned int  eventTimeSize,
                                             double       *eventTime,

                                             double       *response,
                                             double        mean,
                                             double        variance,
                                             unsigned int  maxLevel,

                                             double      **feature,
                                             unsigned int  featureCount);


unsigned int *alloc_uivector(unsigned int nh);
void          dealloc_uivector(unsigned int *v, unsigned int nh);

double       *alloc_dvector(double *v, unsigned int nh);
void          dealloc_dvector(double *v, unsigned int nh);

unsigned int **alloc_uimatrix(unsigned int n2h, unsigned int nh);
void          dealloc_uimatrix(unsigned int **v, unsigned int n2h, unsigned int nh);
