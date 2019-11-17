
/*=============================================================================
cue_comp.cpp
===============================================================================
Main file of a model exploring the evolution communication systems where 
individuals learn to respond to a signal. Individuals play the 
traditional hawk-dove game. 


Written by :

Andr�s E.Qui�ones
Posdoctoral researcher
Departamento de ciencias biol�gicas
Universidad de los Andes
Bogot� Colombia

Start date :
11 June 2018
=============================================================================*/


#include <stdio.h>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <math.h>	
#include <vector>
#include <omp.h>
//#include "tchar.h"   //Eliminate for g++
#include "../Cpp/Routines/C++/RandomNumbers/random.h"
#include "../Cpp/json.hpp"       
#include "../Cpp/Routines/C++/RandomNumbers/utils.h"
// Header for reading and using JSON files see https://github.com/nlohmann/json

using namespace std;
using json = nlohmann::json;

// Classes

enum strategy { hawk, dove, evaluator};

// counters for statistics

int countGenotypes[3];
int countPhenotypes[2];
int countIntTypes[3];
double BadgeMeanSd[2];
double alphaMeanSd[2];
double betaMeanSd[2];
double featActMean[20];
double featCritMean[20];
int countIntTypesGen[3];

class individual {
	public:
		individual(strategy genotype_, double QualStDv,
			double alphaBadge_, double alphaCI, 
			double alphaAI, double gammaI, double sigmaSqI,	int nCenters_,
			double initCrit, double initAct );
		individual(individual& mother, double QualStDv,
			double mutRate,double mutSD, int mutType, double initCrit, 
			double initAct);
		double curr_payoff;
		double cum_payoff;
		int ninterac;
		strategy phenotype;
		strategy get_strat() {
			return(genotype);
		}
		double get_badge() {
			return(own_badge);
		}
		double get_alpha() {
			return(alphaBadge);
		}
		double get_beta() {
			return(betaBadge);
		}
		double get_quality() {
			return(quality);
		}
		double  get_feat(bool act,int id){
			if(act)	return(featWeightsAct[id]);
			else return (featWeightsCrit[id]);
		}
		int get_nCenter() {
			return(nCenters);
		}
		int set_phenotype(individual partner);
		void get_payoff(individual partner, vector<double> param, bool win);
		void set_Badge(double stdDev);
		bool Winfight(double otherQuality, double strQual);
		strategy mutateStr(strategy genotype,double mutRate,int mutType);
		double mutateDoub(double value, double mutRate, double mutSD);
		void calcRespValPref(individual partner);
		void update();
		bool viability(double alphaCost, double betaCost);
	private:
		int nCenters;
		// number of ranges in which the morphological trait range is divided
		vector<double> featWeightsCrit;
		// vector of weights to fit the state value function. 
		// Each corresponds to one value in the morphological trait range
		vector <double> featWeightsAct;
		// vector of weights to fit the action function. 
		// Each corresponds to one value in the morphological trait range
		vector <double> centers;
		// Numerical value in the morphological trait range that corresponds 
		// to the weights in FeatWeightsCrit anf featWeightAct
		vector <double> responses;
		// Response triggered by each weight corrected by distance accordig to RBF
		double valueT; // estimated value for state at time t
		double preferenceT;  // estimated preference for acting as hawk at time t
		double sigmaSq; 
		// degree of generalization
		double alphaCrit;
		// speed of learning for the critic
		double alphaAct;
		// speed of learning for the actor
		double gamma;
		// importance of future rewards 
		strategy genotype;
		double own_badge;
		double quality;
		double alphaBadge;
		double betaBadge;
};

// set quality and badge of status
void individual::set_Badge(double stdDev=0.1) {
	if (stdDev>1)
	{
		quality = rnd::uniform();
	}
	else {
		quality = rnd::normal(0.5, stdDev);
		clip_range(quality, 0, 1);
	}
	own_badge = 1 / (1 + exp(alphaBadge - betaBadge * quality));
}

// constructors
individual::individual(strategy genotype_=hawk, double QualStDv = 0.1,
	double alphaBadge_=0, double alphaCI = 0.05, 
	double alphaAI = 0.05, double gammaI = 0, double sigmaSqI = 0.01, 
	int nCenters_=6,double initCrit=0,double initAct=0) {
	nCenters = nCenters_;
	alphaBadge = alphaBadge_;
	betaBadge = alphaBadge_*2;
	genotype = genotype_;
	set_Badge(QualStDv);
	alphaAct = alphaAI, alphaCrit = alphaCI, gamma = gammaI, sigmaSq = sigmaSqI;
	double interv = 1 / (static_cast<double>(nCenters)-1);
	for (int i = 0; i < nCenters; i++)	{
		centers.emplace_back(interv * i);
		featWeightsAct.emplace_back(initAct);
		featWeightsCrit.emplace_back(initCrit);
		responses.emplace_back(0);
	}
	curr_payoff = 0, cum_payoff = 0, ninterac = 0, valueT = 0;
	preferenceT=0;
}

individual::individual(individual& mother, double QualStDv,
	double mutRate, double mutSD, int mutType, 
	double initCrit = 0, double initAct = 0) {
	genotype = mutateStr(mother.genotype,mutRate,mutType);
	nCenters = mother.nCenters;
	alphaBadge = mutateDoub(mother.alphaBadge,mutRate,mutSD);
	betaBadge = mutateDoub(mother.betaBadge,mutRate,mutSD);
	set_Badge(QualStDv); 
	curr_payoff = 0, cum_payoff = 0, ninterac = 0, valueT = 0, 
	preferenceT=0;
	alphaAct = mother.alphaAct;
	alphaCrit = mother.alphaCrit;
	gamma = mother.gamma;
	sigmaSq = mother.sigmaSq;
	double interv = 1 / (static_cast<double>(nCenters) - 1);
	for (int i = 0; i < nCenters; i++) {
		centers.emplace_back(interv * i);
		featWeightsAct.emplace_back(initAct);
		featWeightsCrit.emplace_back(initCrit);
		responses.emplace_back(0);
	}
}

void individual::calcRespValPref(individual partner) {
	double totValue = 0;
	double totPref = 0;
	for (int countCenters = 0; countCenters < nCenters; ++countCenters) {
		responses[countCenters] = 
			exp(-(pow(abs(partner.get_badge() - centers[countCenters]),2))
			/ (2 * sigmaSq));
		totValue += responses[countCenters] * featWeightsCrit[countCenters];
		totPref += responses[countCenters] * featWeightsAct[countCenters];
	}
	valueT = totValue;
	preferenceT = totPref;
}

double logist(double value1, double value2, double beta=1,double alpha=0) {
	return (1 / (1 + exp(alpha-beta*(value1 - value2))));
}

void individual::update() {
	// change estimated value according to current reward and 
	// estimates of future state-action pair
	double delta = curr_payoff - valueT;
	double p0 = logist(preferenceT,0);
	double eligVec;
	if (phenotype==0) {
		// if phenotype is hawk
		eligVec = -p0;
	}
	else {
		// if phenotype is dove
		eligVec = (1 - p0);
	}
	for (int countCent = 0; countCent < nCenters; ++countCent) {
		featWeightsCrit[countCent] += alphaCrit * delta*
			responses[countCent];
		featWeightsAct[countCent] += alphaAct * delta*eligVec*
			responses[countCent];
	}
}

strategy individual::mutateStr(strategy genotype,double mutRate,int mutType) {
	if (mutType > 0) {
		if (rnd::uniform() < mutRate) {
			strategy newgenotype = (strategy)rnd::integer(mutType);
			// set intput to random generator to 3, to include evaluators
			// set intput to random generator to 2, for simple hawk-dove game
			return(newgenotype);
		}
		else {
			return(genotype);
		}
	}
	else return(genotype);
}

double  individual::mutateDoub(double value, double mutRate, double mutSD) {
	if (rnd::uniform() < mutRate) {
		double newValue = value + rnd::normal(0,mutSD);
		return(newValue);
	}
	else {
		return(value);
	}
}


void individual::get_payoff(individual partner,vector<double> payoff_matrix, 
	bool win) {      
	/*  (This are IDs to the entries of the matrix, NOT payoffs)    
	        Hawk      Dove     
	   Hawk   0         1
	   Dove   2         3
	*/
	int scenario = 2 * phenotype + partner.phenotype;
	if (scenario == 0 && win) {
		curr_payoff = payoff_matrix[1] - payoff_matrix[0] * 0.5;
		cum_payoff += payoff_matrix[1] - payoff_matrix[0] * 0.5;
	}
	else if (scenario == 0) {
		curr_payoff = - payoff_matrix[0] * 0.5;
		cum_payoff += - payoff_matrix[0] * 0.5;
	}
	else {
		curr_payoff = payoff_matrix[scenario];
		cum_payoff += payoff_matrix[scenario];
	}
	++ninterac;
}

bool individual::Winfight(double otherQuality,double strQual) {
	return(rnd::bernoulli(logist(get_quality(), otherQuality, strQual)));
}


int individual::set_phenotype(individual partner) {
	if (get_strat() == evaluator){
		calcRespValPref(partner);
		phenotype = static_cast<strategy>(rnd::bernoulli(logist(preferenceT, 0)));
	}
	else {
		phenotype = get_strat();
	}
	++countPhenotypes[phenotype];
	return(phenotype);
}

bool individual::viability(double alphaCost,double betaCost) {
	return(rnd::binomial(logist(quality-own_badge,betaCost,alphaCost)));
}

void Reprod(vector<individual> &popT, int popsize, double mutRate,
	double mutSD, double baselineFit, int mutType, double QualStDv,
	double initCrit, double initAct,double alphaCost, double betaCost) {
	vector<individual> popTplus1;
	popTplus1.reserve(popsize);
	rnd::discrete_distribution payoff_dist(popsize);
	
	for (vector<individual>::iterator itpop = popT.begin(); 
		itpop < popT.end(); ++itpop) {
		payoff_dist[itpop-popT.begin()] = baselineFit + 
			itpop->cum_payoff/itpop->ninterac;
	}
	for (vector<individual>::iterator itpopTplus1 = popTplus1.begin(); 
		itpopTplus1 < popTplus1.end(); ++itpopTplus1) {
		individual tmp = individual(popT[payoff_dist.sample()], QualStDv, mutRate,
			mutSD, mutType, initCrit, initAct);

		*itpopTplus1 = individual(popT[payoff_dist.sample()], QualStDv,mutRate,
			mutSD, mutType,	initCrit,initAct);
	}
	vector<individual>::iterator itpopTplus1 = popTplus1.begin();
	while (	itpopTplus1 < popTplus1.end()) {
		*itpopTplus1 = individual(popT[payoff_dist.sample()], QualStDv, mutRate,
			mutSD, mutType, initCrit, initAct);
		if (betaCost == 0) ++itpopTplus1;
		else if (itpopTplus1->viability(alphaCost, betaCost)) {
			++itpopTplus1;
		}
	}
	popT = popTplus1;
	/*vector<individual>::iterator itpopTplus1 = popTplus1.begin();
	for (vector<individual>::iterator itpopT = popT.begin();
		itpopT < popT.end(); ++itpopT, ++itpopTplus1) {
		*itpopT = *itpopTplus1;
	}*/
}

void get_stats(vector<individual> &popT, int popsize,int nFeat=5){
	countGenotypes[0] = 0;
	countGenotypes[1] = 0; 
	countGenotypes[2] = 0; 
	BadgeMeanSd[0] = 0;
	BadgeMeanSd[1] = 0;
	alphaMeanSd[0] = 0;
	alphaMeanSd[1] = 0;
	betaMeanSd[0] = 0;
	betaMeanSd[1] = 0;
	for(int countFeat = 0; countFeat<nFeat;++countFeat){
		featActMean[countFeat] = 0, featCritMean[countFeat] = 0;
	}
	for (vector<individual>::iterator itpop = popT.begin(); 
		itpop < popT.end(); ++itpop) {
		++countGenotypes[itpop->get_strat()];
		BadgeMeanSd[0] += itpop->get_badge();
		BadgeMeanSd[1] += pow(itpop->get_badge(), 2);
		alphaMeanSd[0] += itpop->get_alpha();
		alphaMeanSd[1] += pow(itpop->get_alpha(),2);
		betaMeanSd[0] += itpop->get_beta();
		betaMeanSd[1] += pow(itpop->get_beta(), 2);
		for(int countFeat = 0; countFeat<nFeat;++countFeat){
			featActMean[countFeat] += itpop->get_feat(1,countFeat);
			featCritMean[countFeat] += itpop->get_feat(0, countFeat);
		}
	}
}

void printLearnDynamics(ofstream &genoutput, vector<individual> &pop,
	int generat, int countInt, int indId, int seed) {
	genoutput << seed << '\t' << generat << '\t' << countInt << '\t' <<
		indId << '\t' << countIntTypesGen[0] << '\t' <<
		countIntTypesGen[1] << '\t' << countIntTypesGen[2] << '\t';
	for (int countFeat = 0; countFeat < pop[indId].get_nCenter();
		++countFeat) {
		genoutput << pop[indId].get_feat(1, countFeat) << '\t'
			<< pop[indId].get_feat(0, countFeat) << '\t';
	}
	genoutput << pop[indId].get_quality() << '\t' << pop[indId].get_strat() <<
		'\t' << pop[indId].get_alpha() << '\t' << pop[indId].get_beta() << '\t'
		<< pop[indId].get_badge() << '\t' << pop[indId].ninterac << '\t';
	genoutput << endl;
}

void interactions(vector<individual> &population, ofstream &genoutput,int nint,
	vector<double> payoff_matrix, double strQual, bool trackPopLearn,
	int printLearnInt, int sampleSize,int generat, int seed, int nIntGroup) {
	int ind1 = population.size();
	int ind2 = population.size();
	int intType;
	bool ind1win;
	vector<int>::iterator itSamp1;
	vector<int> sample;
	if (trackPopLearn) {
		countPhenotypes[0] = 0, countPhenotypes[1] = 0;
		countIntTypes[0] = 0, countIntTypes[1] = 0, countIntTypes[2] = 0;
		countIntTypesGen[0], countIntTypesGen[1], countIntTypesGen[2] = 0;
		for (int countSam = 0; countSam < sampleSize; ++countSam) {
			sample.emplace_back(rnd::integer(population.size()));
		}
	}
	for (int i = 0; i < nint*population.size(); ++i) {
		ind1 = rnd::integer(population.size());
		ind2 = ind1+1+rnd::integer(nIntGroup-1);
		if (ind2 >= population.size()) ind2 = ind2 - population.size();
		intType = 0;
		intType += population[ind1].set_phenotype(population[ind2]);
		intType += population[ind2].set_phenotype(population[ind1]);
		if (trackPopLearn) { 
			int foundInd = 0;
			++countIntTypes[intType], ++countIntTypesGen[intType];
			itSamp1 = find(sample.begin(), sample.end(), ind1);
			if (itSamp1 != sample.end() &&
				(population[*itSamp1].ninterac % printLearnInt) == 0) {
					printLearnDynamics(genoutput, population, generat, i,
						ind1,seed);
					++foundInd;
			}
			itSamp1 = find(sample.begin(), sample.end(), ind2);
			if (itSamp1 != sample.end() &&
				(population[*itSamp1].ninterac % printLearnInt) == 0) {
				printLearnDynamics(genoutput, population, generat, i,
					ind2,seed);
				++foundInd;
			}
			if (foundInd > 0) {
				countIntTypesGen[0]=0, countIntTypesGen[1]=0, 
					countIntTypesGen[2] = 0;
			}
		}
		ind1win = population[ind1].Winfight(population[ind2].get_quality(),
			strQual);
		population[ind1].get_payoff(population[ind2], payoff_matrix,ind1win);
		population[ind2].get_payoff(population[ind1], payoff_matrix,!ind1win);
		population[ind1].update();
		population[ind2].update();
		ind1 = population.size(), ind2 = population.size();
	}
}

double calcSd(double sum[], double invN) {
	return(sqrt(sum[1] * invN -
		pow((sum[0] * invN), 2)));
}

void printStats(int popsize,ofstream &evolOutput, 
	int time, int seed,	int nFeat = 5) {
	double invertTotInt = 1/(static_cast<double>(countPhenotypes[0]) + 
		static_cast<double>	(countPhenotypes[1]));
	double invertPopsize = 1/static_cast<double>(popsize);
	double invertNlearners;
	if (countGenotypes[2] != 0) {
		invertNlearners = 1 / static_cast<double>(countGenotypes[2]);
	}
	else {
		invertNlearners = 0;
	}
	double badgSD = calcSd(BadgeMeanSd, invertPopsize);
	double alphaSD = calcSd(alphaMeanSd, invertPopsize);
	double betaSD = calcSd(betaMeanSd, invertPopsize);
	evolOutput << seed << '\t';
	evolOutput << time << '\t';
	evolOutput << countGenotypes[hawk]      * invertPopsize << '\t';
	evolOutput << countGenotypes[dove]      * invertPopsize << '\t';
	evolOutput << countGenotypes[evaluator] * invertPopsize << '\t';
	evolOutput << countPhenotypes[hawk]     * invertTotInt << '\t';
	evolOutput << countPhenotypes[dove]     * invertTotInt << '\t';
	evolOutput << (double)(countIntTypes[0]) * 2 * invertTotInt << '\t';
	evolOutput << (double)(countIntTypes[1]) * 2 * invertTotInt << '\t';
	evolOutput << (double)(countIntTypes[2]) * 2 * invertTotInt << '\t';
	evolOutput << BadgeMeanSd[0]            * invertPopsize << '\t';
	evolOutput << badgSD << '\t';
	evolOutput << alphaMeanSd[0]            * invertPopsize << '\t';
	evolOutput << alphaSD << '\t';
	evolOutput << betaMeanSd[0]             * invertPopsize << '\t';
	evolOutput << betaSD <<  '\t';
	for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
		evolOutput << featActMean[countFeat] * invertNlearners << '\t';
		evolOutput << featCritMean[countFeat] * invertNlearners << '\t';
	}
	evolOutput << endl;
	
	//for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
	//	cout << featActMean[countFeat] * invertPopsize << '\t';
	//	cout << featCritMean[countFeat] * invertPopsize << '\t';
	//}
	//cout << endl;
}

void printPopSample(vector<individual> &population, ofstream &popOutput,
	int time, int seed, int sampleSize, int nFeat = 5) {
	int sample;
	for (int countSample = 0; countSample < sampleSize; ++countSample) {
		sample = rnd::integer(population.size());
		popOutput << seed << '\t';
		popOutput << time << '\t';
		popOutput << sample << '\t';
		popOutput << population[sample].get_quality() << '\t';
		popOutput << population[sample].get_strat() << '\t';
		popOutput << population[sample].get_alpha() << '\t';
		popOutput << population[sample].get_beta() << '\t';
		popOutput << population[sample].get_badge() << '\t';
		popOutput << population[sample].ninterac << '\t';
		for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
			popOutput << population[sample].get_feat(1,countFeat) << '\t';
			popOutput << population[sample].get_feat(0,countFeat) << '\t';
		}
		popOutput << endl;
	}
}

string create_filename(std::string filename, json param) {
	// name the file with the parameter specifications
	filename.append("_HH");
	filename.append(douts(param["payoff_matrix"][0]));
	filename.append("_HD");
	filename.append(douts(param["payoff_matrix"][1]));
	filename.append("_DH");
	filename.append(douts(param["payoff_matrix"][2]));
	filename.append("_DD");
	filename.append(douts(param["payoff_matrix"][3]));
	filename.append("_");
	std::string namParam = param["namParam"];
	filename.append(namParam);
	filename.append(douts(param[namParam]));
	filename.append(".txt");
	return(filename);
}
void initializeFiles(ofstream &evolOutput, //ofstream &popOutput, 
	ofstream &indOutput, json param) {
	std::string filename = param["folder"];
	filename.append("evolLearn");
	// File to print evolutionary dynamics
	std::string evolFile = create_filename(filename,param);
	/*std::string namefile ="popLearn";
	namedir.append(namefile);*/
	evolOutput.open(evolFile.c_str());
	evolOutput << "seed" << '\t';
	evolOutput << "time" << '\t' << "freqGenHawks" << '\t' << "freqGenDove";
	evolOutput << '\t' << "freqGenEval" << '\t' << "freqFenHawks" << '\t';
	evolOutput << "freqFenDoves" << '\t' << "freqHH" << '\t' << "freqHD" << '\t';
	evolOutput << "freqDD" << '\t' << "meanCue" << '\t' << "sdCue" << '\t';
	evolOutput << "meanAlpha" << '\t' << "sdAlpha" << '\t' << "meanBeta" << '\t';
	evolOutput << "sdBeta" << '\t';
	for(int countFeat=0;countFeat<param["nCenters"];++countFeat){
		evolOutput << "WeightAct_" + itos(countFeat) << '\t';
		evolOutput << "WeightCrit_" + itos(countFeat) << '\t';
	}
	evolOutput << endl;
	// File to print a sample of populations
	/*std::string filename1 = param["folder"];
	filename1.append("popLearn");
	std::string popFile = create_filename(filename1, param);
	popOutput.open(popFile.c_str());
	popOutput << "seed" << '\t';
	popOutput << "time" << '\t' << "idInd" << '\t' << "Quality";
	popOutput << '\t' << "genotype" << '\t' << "alpha" << '\t';
	popOutput << "beta" << '\t' << "Badge" << '\t' << "nInteract" << '\t';
	for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
		popOutput << "WeightAct_" + itos(countFeat) << '\t';
		popOutput << "WeightCrit_" + itos(countFeat) << '\t';
	}
	popOutput << endl;*/
	// File to print a learning dynamics within a generation
	std::string filename1 = param["folder"];
	filename1.append("indLearn");
	std::string IndFile = create_filename(filename1, param);
	indOutput.open(IndFile.c_str());
	indOutput << "seed" << '\t' <<  "time" << '\t' << "numInter" << '\t' 
		<< "indId" << '\t' << "nint_HH" << '\t' <<
		"nint_HD" << '\t' << "nint_DD" << '\t';
	for (int countFeat = 0; countFeat < param["nCenters"];
		++countFeat) {
		indOutput << "WeightAct_" + itos(countFeat) << '\t'
			<< "WeightCrit_" + itos(countFeat) << '\t';
	}
	indOutput << "Quality" << '\t' << "genotype" << '\t' << "alpha" << '\t';
	indOutput << "beta" << '\t' << "Badge" << '\t' << "nInteract" << '\t';
	indOutput << endl;
	//for (int countFeat = 0; countFeat < nFeat; ++countFeat) {
	//	cout << "WeightAct_" + itos(countFeat) << '\t';
	//	cout << "WeightCrit_" + itos(countFeat) << '\t';
	//}
	//cout << endl;
}

int main(int argc, char* argv[]){

	mark_time(1);

	 //uncomment for debugging
	//json param;
	//param["totGen"]            = 10;   // Total number of generations
	//param["nRep"]              = 5;     // Number of replicates
	//param["printGen"]          = 10;     // How often data is printed	
	//param["printLearn"]        = 10;	  // how often learning dyn are printed
	//param["printLearnInt"]     = 20;   // How often are learning parameters printed
	//param["init"]              = {0,0,1};        //Initial frequencies
	//param["payoff_matrix"]     = {1.5,1,0,0.5};  
	//param["popSize"]           = 1000;
	//param["MutSd"]             = 0.1;
	//param["nInt"]              = 50;    // Number of interactions per individual
	//param["mutRate"]           = 0.001;
	//param["strQual"]           = 10;
	//param["baselineFit"]       = 1;
	//param["mutType"]		     = 2;  
	//// How many strategies are introduced by mutation
	//param["sampleSize"]        = 20; 
	//param["alphaBad"]			 = 0;
	//param["betaBad"]			 = 0;
	//param["alphaCrit"]     	 = 0.01;
	//param["alphaAct"]     	 = 0.01;
	//param["sigSq"]        	 = 0.01;
	//param["nCenters"]     	 = 6;
	//param["initCrit"]          = 0;
	//param["initAct"]           = 5;
	//param["QualStDv"]          = 0.1;
	//param["nIntGroup"]		 = 50;
	//param["initAct"]			 =-3;
	//param["betCost"]           = -3;
	//param["alphCost"]			 = 3;
	//param["namParam"]          = "baselineFit";  
	//// which parameter to vary inside the program
	//param["rangParam"]         = { 0.2 }; 
	//// range in which the paramenter varies
	//param["folder"]            = "E:/Proyectos/SocialComp_morphCue/Simulations/test_/";
	
		
	// Comment for debugging
	ifstream input(argv[1]);
	if (input.fail()) { cout << "JSON file failed" << endl; }
	nlohmann::json param = json::parse(input);

	string namParam = param["namParam"];

	
	for (json::iterator itParVal = param["rangParam"].begin();
		itParVal != param["rangParam"].end(); ++itParVal) {
		param[namParam] = *itParVal;
		ofstream  evolOutput, indOutput;//popOutput,
		initializeFiles(evolOutput,indOutput,param);//popOutput,
		#pragma omp parallel for
		for (int seed = 0; seed < param["nRep"]; ++seed) {
			cout << param["namParam"] << "=" << *itParVal << "	" << 
				"seed=" << seed << endl;
			vector<individual> population;
			population.reserve(param["popSize"]);
			// intial conditions
			rnd::discrete_distribution initFreq(3);
			for (json::iterator initIt = param["init"].begin();
				initIt != param["init"].end(); ++initIt) {
				initFreq[initIt - param["init"].begin()] = *initIt;
			}
			for (int popId = 0; popId < param["popSize"]; ++popId) {
				population.push_back(individual((strategy)initFreq.sample(),
					param["QualStDv"], param["alphaBad"],	
					param["alphaCrit"],	param["alphaAct"],0, param["sigSq"], 
					param["nCenters"],param["initCrit"],param["initAct"]));
			}
			for (int generation = 0; generation < param["totGen"]; 
				++generation) {
				interactions(population, indOutput, param["nInt"],
					param["payoff_matrix"], param["strQual"],
					generation % static_cast<int>(param["printLearn"]) == 0,
					param["printLearnInt"], param["sampleSize"], generation, seed, 
					param["nIntGroup"]);
				#pragma omp critical
				{
					if (generation % static_cast<int>(param["printGen"]) == 0) {
						//cout << "time=" << generation << endl;
						get_stats(population, param["popSize"], param["nCenters"]);
						printStats(param["popSize"], evolOutput, generation, seed, param["nCenters"]);
						/*printPopSample(population, popOutput, generation, seed,
							param["sampleSize"],param["nCenters"]);*/
					}
				}
				Reprod(population, param["popSize"], param["mutRate"],
					param["MutSd"], param["baselineFit"],param["mutType"],
					param["QualStDv"],param["initCrit"], param["initAct"], 
					param["alphCost"], 
					param["betCost"]);
				
			}
		
		}
		//popOutput.close();
		evolOutput.close();
		indOutput.close();
	}
	
	mark_time(0);
	//wait_for_return();

	return 0;
}
