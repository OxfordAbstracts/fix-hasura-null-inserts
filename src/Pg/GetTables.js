import pgStructure from "pg-structure";
// console.log({pgStructure})

export function getTablesImpl({ url }) {
  return async () => {
    const db = await pgStructure.default(url);
    const tables = db.get("public").tables;

    return tables.map(table => { 

        const primaryKeyCols = table.primaryKey?.columns || [];
        
        const primaryColNames = new Set(primaryKeyCols.map(col => col.name));
        // console.log({primaryColNames})
        const columns = Array.from(table.columns.map(column => {
            return { 
                name: column.name,
                nullable: column.defaultWithTypeCast || (!column.notNull && !primaryColNames.has(column.name)),
            }
        }))

        return { name: table.name, columns}
    });
  };
}
