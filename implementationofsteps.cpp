//author: Marzieh Ranjbar Pirbasti
#include <vector>
#include <iostream>
#include <algorithm>
#include <stack>

using namespace std;

vector<double> relative_speed;
vector<int> assignments;
vector<vector<double>> speed_links;
vector<vector<double>> latency_links;


//step 1
vector<pair<double,vector<int> > > transform (vector<pair<double,vector<pair<int,double> > > > G){
    vector<pair<double,vector<int> > > Gp;
    for (int i=0;i<G.size();i++){

        double weight = G[i].first/relative_speed[assignments[i]];
        vector<int> connections;
        for (int j=0;j<G[i].second.size();j++){
            connections.push_back(G[i].second[j].first);
        }
        Gp.push_back(make_pair(weight,connections));
    }
    //NOW we need to add additional edges for communication:
    for (int i=0;i<G.size();i++){

        for (int j=0;j<G[i].second.size();j++){
            if (assignments[i] != assignments[G[i].second[j].first]){
                //GP edge needs to be modified
                //first insert a new node with one outgoing edge to G[i].second[j].first, its weight being the second/speed + latency
                double weight = latency_links[assignments[i]][assignments[G[i].second[j].first]] + G[i].second[j].second/speed_links[assignments[i]][assignments[G[i].second[j].first]]*8;
                //then, modify the connection to instead of going to G[i].second[j].first, go to this one, also with proper weight.
                for (int k=0;k<Gp[i].second.size();k++)
                if (Gp[i].second[k] == G[i].second[j].first)
                    Gp[i].second[k] = Gp.size();//new node
                vector<int> connections;
                connections.push_back(G[i].second[j].first);
                Gp.push_back(make_pair(weight,connections));
                
            }
        }
    }
    

return Gp;
}

//step 2
vector<vector<int> > itemize (vector<pair<double,vector<int> > > GP){

    vector<vector<int> > L;
    L.resize(relative_speed.size()+speed_links.size()*speed_links.size()-speed_links.size());
    vector<double> time(L.size(),0);
    vector<int> task(L.size(),-1);
    for (int i=0;i<L.size();i++){
        L[i].resize(0);
        L[i].clear();
        }
    vector<int> N;
    for (int i=0;i<GP.size();i++)
        N.push_back(i);
    while (N.size() > 0){
        for (int i=0;i<N.size();i++){
            if (task[assignments[N[i]]] == - 1){
                bool has_dependents = false;
                for (int j=0;j<N.size();j++){
                    if (N[i]==N[j]) continue;
                    for (int z = 0;z<GP[N[j]].second.size();z++)
                    if (GP[N[j]].second[z]== N[i])
                        has_dependents = true;
                }
                if (!has_dependents){
                    task[assignments[N[i]]] = N[i];
                    time[assignments[N[i]]] = GP[N[i]].first;
                    L[assignments[N[i]]].push_back(N[i]);
                }


            }
        }

        double step_size = numeric_limits<double>::max();
        for (int j=0;j<L.size();j++){
            if (task[j] != -1 && time[j] > 0.0)
                step_size = min(step_size, time[j]);
        }
        for (int j=0;j<L.size();j++){
            if (time[j] > 0.0)
                time[j] -= step_size;
        }

        for (int j=0;j<N.size();j++){
            if (task[assignments[N[j]]] == N[j] && time[assignments[N[j]]] == 0){
                task[assignments[N[j]]] = -1;
            swap(N[j],N[N.size()-1]);
            N.pop_back();
            }
        }

    }


    return L;
}

//step 3 
vector<pair<double,vector<int> > > insert_dep (vector<pair<double,vector<int> > > GP, vector<vector<int> > L){
    for (int i=0;i<L.size();i++){
        for (int j=0;j<L[i].size();j++){
            if (j==L[i].size()-1 || L[i].size()==0) continue;
            bool exists = false;
            for (int k=0;k<GP[L[i][j]].second.size();k++){
                if (GP[L[i][j]].second[k] == L[i][j+1])
                    exists = true;
            }

            if (!exists){
                GP[L[i][j]].second.push_back(L[i][j+1]);
                }
        }

    }
    return GP;
}

void helper(int v, bool visited[], stack<int>& ms,vector<pair<double,vector<int> > >  GP)
{
    visited[v] = true;
 
    for (int i=0;i<GP[v].second.size();i++)
        if (!visited[GP[v].second[i]])
            helper(GP[v].second[i], visited, ms, GP);
 
    ms.push(v);
}

//top sort
vector<int> sorted(vector<pair<double,vector<int> > > GP){
    vector<int> result;
    stack<int> ms;
    bool* visited = new bool[GP.size()];
    for (int i = 0; i < GP.size(); i++)
        visited[i] = false;
 
    for (int i = 0; i < GP.size(); i++)
        if (visited[i] == false)
            helper(i, visited, ms, GP);
 
    while (ms.empty() == false) {
        result.push_back(ms.top());
        ms.pop();
    }
    return result;
}
//step 4
vector<double> LP (vector<pair<double,vector<int> > > GP){

vector<int> S=sorted(GP);
vector<double> lp;
for (int i=0;i<GP.size();i++)
lp.push_back(0);


for (int i=0;i<S.size();i++){
    for (int j=0;j<GP[S[i]].second.size();j++)
        lp[GP[S[i]].second[j]] = max(lp[GP[S[i]].second[j]],lp[S[i]]+GP[S[i]].first);

}


return lp;

}
//step 5
vector<vector<double> >final_step (vector<pair<double,vector<int> > > GP, vector<double> lp, double fault_size){

    vector<vector<double>> final_result;
    vector<double> rp;
    for (int i=0;i<GP.size();i++)
        rp.push_back(0);

    vector<int> S=sorted(GP);

    for (int i=S.size()-1;i>=0;i--){
        //find all edges going INTO S[i]
        //update the values of those source nodes
        for (int j=0;j<GP.size();j++){
            for (int z =0;z<GP[j].second.size();z++){
                if (GP[j].second[z] == S[i]){
                    //found it.


                    rp[j] = max(rp[j],rp[S[i]]+GP[S[i]].first);
                    

                }

            }
        }



    }
    double largest = 0;
    vector<double> h;
    for (int i=0;i<GP.size();i++){
        h.push_back( lp[i] + rp[i] + GP[i].first);
        largest = max(largest,h[i]);
    }
    for (int i=0;i<GP.size();i++){
        h[i] = largest - h[i];
        double ww = max(0.,fault_size + GP[i].first - h[i]);
        double bb = max(0.,fault_size-h[i]);
        double ee = 0;
        if (h[i] <= fault_size)
            ee = GP[i].first/2 + fault_size - h[i];
        else{
        ee = max(0.,(GP[i].first + fault_size - h[i])/2);
        ee *= (GP[i].first + fault_size - h[i])/(GP[i].first);}

        vector<double> res;
        res.push_back(h[i]);
        res.push_back(ww);
        res.push_back(ee);
        res.push_back(bb);
        final_result.push_back(res);
    }

    


    return final_result;
}


int main(){
vector<pair<double,vector<pair<int,double> > > > G;

pair<int,double> edge;
vector<pair<int,double> > edges;
pair<double,vector<pair<int,double> > > node;

/*BELOW ARE THE USER PROVIDED INPUTS
THIS SAMPLE IS FOR THE FACE RECOGNITION APPLICATION*/
//relative speed of compute resources, here we go with 1 and 3
relative_speed.push_back(1);
relative_speed.push_back(3);
//assignments of tasks to resources
assignments.push_back(1);
assignments.push_back(1);
assignments.push_back(1);
assignments.push_back(1);
assignments.push_back(0);
assignments.push_back(0);
assignments.push_back(1);
assignments.push_back(0);
assignments.push_back(0);
assignments.push_back(1);
assignments.push_back(0);
assignments.push_back(0);
assignments.push_back(1);
assignments.push_back(0);
assignments.push_back(1);
assignments.push_back(2);
assignments.push_back(3);

//speed and latency of links
speed_links.resize(2);
for (int i=0;i<speed_links.size();i++)
    speed_links[i].resize(2);

latency_links.resize(2);
for (int i=0;i<latency_links.size();i++)
    latency_links[i].resize(2);

speed_links[0][0] = 0;
speed_links[1][0] = 150;
speed_links[0][1] = 50;
speed_links[0][1] = 50;

latency_links[0][0] = 100;
latency_links[1][0] = 100;
latency_links[0][1] = 100;
latency_links[1][1] = 100;

/*BELOW IS THE DESCRIPTION OF THE WORKFLOW GRAPH, IN THIS CASE THE FACE RECOGNITION APPLICATION*/
//0
edge.first = 3;
edge.second = 12.003;
edges.push_back(edge);
G.push_back(make_pair(68.6,edges));
edges.clear();
//1
edge.first = 3;
edge.second = 12;
edges.push_back(edge);
G.push_back(make_pair(33,edges));
edges.clear();
//2
edge.first = 3;
edge.second = 0.3;
edges.push_back(edge);
G.push_back(make_pair(2.2,edges));
edges.clear();
//3
edge.first = 6;
edge.second = 19.806;
edges.push_back(edge);
G.push_back(make_pair(516.6,edges));
edges.clear();
//4
edge.first = 7;
edge.second = 0;
edges.push_back(edge);
G.push_back(make_pair(77.7,edges));
edges.clear();
//5
edge.first = 9;
edge.second = 10.206;
edges.push_back(edge);
G.push_back(make_pair(192,edges));
edges.clear();
//6
edge.first = 9;
edge.second = 10.204;
edges.push_back(edge);
G.push_back(make_pair(516.5,edges));
edges.clear();
//7
edge.first = 10;
edge.second = 0.6752;
edges.push_back(edge);
G.push_back(make_pair(75.2,edges));
edges.clear();
//8
edge.first = 11;
edge.second = 0;
edges.push_back(edge);
G.push_back(make_pair(2.2,edges));
edges.clear();
//9
edge.first = 12;
edge.second = 10.206;
edges.push_back(edge);
G.push_back(make_pair(722.2,edges));
edges.clear();
//10
edge.first = 13;
edge.second = 1.0242;
edges.push_back(edge);
G.push_back(make_pair(80.7,edges));
edges.clear();
//11
edge.first = 13;
edge.second = 0.6;
edges.push_back(edge);
G.push_back(make_pair(35.9,edges));
edges.clear();
//12
edge.first = 14;
edge.second = 0.0002;
edges.push_back(edge);
G.push_back(make_pair(1464,edges));
edges.clear();
//13
edge.first = 14;
edge.second = 0.00029;
edges.push_back(edge);
G.push_back(make_pair(137.8,edges));
edges.clear();
//14
G.push_back(make_pair(1553.3,edges));
edges.clear();


/*actual invokation of the steps*/
//step 1
vector<pair<double,vector<int> > > GP = transform(G);
//step 2
vector<vector<int> > L = itemize(GP);
//step 3
GP = insert_dep(GP,L);
//step 4
vector<double> ll = LP(GP);
//step 5
vector<vector<double> >res = final_step(GP,ll,200);

//print the final results for the user
for (int i=0;i<res.size();i++)
{
    cout<<i<<": ";
    for (int j=0;j<res[i].size();j++)
    {
        cout<<res[i][j]<< " ";
    }
    cout<<endl;
}
return 0;
}
