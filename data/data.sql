type: mysql
user: stacktest
password: BUjT4CJrbfSdVNZe
host: localhost
dbname: stacktest
query: SELECT a.subject_id, a.rank_presented, a.user_rating, b.rep_shown, b.priorknowledge, c.score, c.userrep, c.length, c.containscode FROM testdata_answer a INNER JOIN testdata_question b ON a.testdata_question_id = b.id INNER JOIN answer c ON a.answer_id = c.id