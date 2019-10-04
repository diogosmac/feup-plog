cargo(tecnico, rogerio).
cargo(tecnico, ivone).
cargo(engenheiro, daniel).
cargo(engenheiro, isabel).
cargo(engenheiro, oscar). 
cargo(engenheiro, tomas).
cargo(engenheiro, ana).
cargo(supervisor, luis).
cargo(supervisor_chefe, sonia).
cargo(secretaria_exec, laura).
cargo(diretor, santiago).

chefiado_por(tecnico, engenheiro).
chefiado_por(engenheiro, supervisor).
chefiado_por(analista, supervisor).
chefiado_por(supervisor, supervisor_chefe).
chefiado_por(supervisor_chefe, director).
chefiado_por(secretaria_exec, director). 


# a) Quem e que diretamente chefia tecnicos, e tem alguem a chefia-los?
# b) Ha algum cargo que a Ivone desempenhe, e que chefie tecnicos?
# c) Quem e supervisor?
# d) Quais sao os cargos (e as pessoas que os desempenham) que sao chefiados
#    por supervisores chefe? No caso de nao haver, quais os chefiados por
#    supervisores normais?
# e) Quais sao os cargos diretamente chefiados pelo diretor, e que nao sao
#    desempenhados pela Carolina?