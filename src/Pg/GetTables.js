import pgStructure from "pg-structure";
// console.log({pgStructure})

export function getTablesImpl({ url }) {
  return async () => {
    const db = await pgStructure.default(url);
    const schema = db.get("public");

    const tables = schema.tables;

    return tables.map(table => { 

        const primaryKeyCols = table.primaryKey?.columns || [];
        
        const primaryColNames = new Set(primaryKeyCols.map(col => col.name));

        const columns = Array.from(table.columns.map(column => {
            // console.log(column?.parent?.triggers)
            return { 
                name: column.name,
                nullable: !(column.defaultWithTypeCast || column.notNull || primaryColNames.has(column.name)),
            }
        }))

        return { name: table.name, columns}
    });
  };
}
