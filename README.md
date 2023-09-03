# prolog-blocks-world
Planejador implementado para resolver mundo dos blocos. Utiliza Iterative deepening depth-first search buscar a melhor solução com a menor quantidade de movimentos possíveis. Implementado em SWI-Prolog, baseado em https://stackoverflow.com/questions/41025065/prolog-strips-planner-never-completes.

Para executar os testes, utilize o programa swiprolog e execute as linhas abaixo: ?- [main], run_testes().

Para executar um estado inicial customizado: ?- [main], run(EstadoInicial, EstadoFinal).

Onde EstadoInicial e EstadoFinal são uma lista de relações que descrevem o estado atual.
